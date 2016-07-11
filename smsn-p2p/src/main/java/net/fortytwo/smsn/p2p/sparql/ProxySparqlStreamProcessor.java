package net.fortytwo.smsn.p2p.sparql;

import net.fortytwo.smsn.p2p.Connection;
import net.fortytwo.smsn.p2p.MessageHandler;
import net.fortytwo.stream.BasicSubscription;
import net.fortytwo.stream.sparql.RDFStreamProcessor;
import org.json.JSONArray;
import org.json.JSONException;
import org.json.JSONObject;
import org.openrdf.model.Statement;
import org.openrdf.model.Value;
import org.openrdf.model.impl.ValueFactoryImpl;
import org.openrdf.query.BindingSet;

import java.io.IOException;
import java.util.HashMap;
import java.util.Map;
import java.util.function.BiConsumer;
import java.util.function.Supplier;
import java.util.logging.Logger;

/**
 * @author Joshua Shinavier (http://fortytwo.net)
 */
public class ProxySparqlStreamProcessor extends RDFStreamProcessor<String, ProxySparqlStreamProcessor.Query> {
    protected static final Logger logger = Logger.getLogger(ProxySparqlStreamProcessor.class.getName());

    // tags
    public static final String
            TAG_RDF_DATA = "rdf-data",
            TAG_SPARQL_QUERY = "sparql-query",
            TAG_SPARQL_RESULT = "sparql-result";

    // fields
    public static final String
            MAPPING = "mapping",
            DATASET = "dataset",
            EXPIRATION_TIME = "expirationTime",
            QUERY = "query",
            QUERY_ID = "id",
            SOLUTION = "solution",
            TTL = "ttl";

    private final Connection connection;
    private final SimpleJSONRDFFormat jsonrdfFormat;

    private final Map<String, Query> queriesById;
    private final Map<String, BiConsumer<BindingSet, Long>> handlers;

    public ProxySparqlStreamProcessor(final Connection connection) {
        this.connection = connection;

        jsonrdfFormat = new SimpleJSONRDFFormat(new ValueFactoryImpl());

        queriesById = new HashMap<>();
        handlers = new HashMap<>();

        connection.registerHandler(TAG_SPARQL_RESULT, new MessageHandler() {
            public void handle(final JSONObject result) {
                try {
                    handleSparqlResultJSON(result);
                } catch (SimpleJSONRDFFormat.ParseError e) {
                    logger.warning("invalid SPARQL query result: " + result);
                    e.printStackTrace(System.err);
                }
            }
        });
    }

    public void clear() {
        throw new UnsupportedOperationException("don't have rights to clear remote query engine");
    }

    public void notifyConnectionOpen() throws IOException {
        // send all subscriptions, again if necessary
        for (Map.Entry<String, Query> e : queriesById.entrySet()) {
            sendSubscriptionMessage(e.getValue().queryStr, e.getKey(), e.getValue().ttl);
        }
    }

    @Override
    protected BasicSubscription<String, Query, BindingSet> createSubscription(final int ttl,
            final String sparqlQuery, BiConsumer<BindingSet, Long> consumer)
            throws IOException {


        Query query = new Query();
        query.queryStr = sparqlQuery;
        query.ttl = ttl;

        BasicSubscription<String, Query, BindingSet> sub
                = new BasicSubscription<>(sparqlQuery, query, consumer, this);
        queriesById.put(sub.getId(), query);

        if (connection.isActive()) {
            sendSubscriptionMessage(sparqlQuery, sub.getId(), ttl);
        }

        handlers.put(sub.getId(), consumer);

        return sub;
    }

    @Override
    protected boolean addTuple(Value[] tuple, int ttl, long now) {
        return false;
    }

    @Override
    public void unregister(BasicSubscription<String, Query, BindingSet> subscription) throws IOException {
        // TODO
        throw new UnsupportedOperationException("not yet possible to cancel subscriptions through the proxy");
    }

    @Override
    protected String parseQuery(String queryStr) throws InvalidQueryException, IncompatibleQueryException {
        return queryStr;
    }

    @Override
    public boolean renew(BasicSubscription<String, Query, BindingSet> subscription, int i) throws IOException {
        // TODO
        throw new UnsupportedOperationException("not yet possible to renew subscriptions through the proxy");
    }

    public void addStatements(int ttl, Statement... statements) throws IOException {
        try {
            JSONArray a = jsonrdfFormat.statementsToJSON(statements);

            sendDatasetMessage(a, ttl);
        } catch (JSONException e) {
            throw new IOException(e);
        }
    }

    @Override
    public void setClock(Supplier<Long> clock) {
        throw new UnsupportedOperationException("sorry, the clock cannot be set by proxy");
    }

    private void handleSparqlResultJSON(final JSONObject result) throws SimpleJSONRDFFormat.ParseError {
        try {
            String queryId = result.getString(QUERY_ID);

            BiConsumer<BindingSet, Long> handler = handlers.get(queryId);

            if (null != handler) {
                JSONObject mapping = result.getJSONObject(MAPPING);
                Long expirationTime = result.getLong(EXPIRATION_TIME);

                BindingSet bindingSet = jsonrdfFormat.toBindingSet(mapping);

                // note: no need to catch runtime exceptions here; the connection will survive them
                handler.accept(bindingSet, expirationTime);
            }
        } catch (JSONException e) {
            throw new SimpleJSONRDFFormat.ParseError(e);
        }
    }

    private void sendSubscriptionMessage(final String query,
                                         final String queryId,
                                         final long ttl) throws IOException {
        try {
            JSONObject j = new JSONObject();
            j.put(QUERY_ID, queryId);
            j.put(QUERY, query);
            j.put(TTL, ttl);

            // TODO: confirmation of subscription receipt, retry in case of failure
            // queries are of central importance and should be buffered to ensure that they are received
            connection.sendBuffered(TAG_SPARQL_QUERY, j);
        } catch (JSONException e) {
            throw new IOException(e);
        }
    }

    private void sendDatasetMessage(final JSONArray statements, final long ttl) throws JSONException, IOException {
        JSONObject j = new JSONObject();
        j.put(DATASET, statements);
        j.put(TTL, ttl);

        // send RDF data immediately or not at all; don't buffer
        connection.sendNow(TAG_RDF_DATA, j);
    }

    public static class Query {
        public String queryStr;
        public long ttl;
    }
}
