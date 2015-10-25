package net.fortytwo.smsn.p2p.sparql;

import edu.rpi.twc.sesamestream.BindingSetHandler;
import edu.rpi.twc.sesamestream.QueryEngine;
import edu.rpi.twc.sesamestream.Subscription;
import net.fortytwo.smsn.SemanticSynchrony;
import net.fortytwo.smsn.p2p.Connection;
import net.fortytwo.smsn.p2p.MessageHandler;
import org.json.JSONArray;
import org.json.JSONException;
import org.json.JSONObject;
import org.openrdf.model.Statement;
import org.openrdf.model.impl.ValueFactoryImpl;
import org.openrdf.query.BindingSet;

import java.io.IOException;
import java.util.HashMap;
import java.util.Map;
import java.util.logging.Logger;

/**
 * @author Joshua Shinavier (http://fortytwo.net)
 */
public class QueryEngineProxy implements QueryEngine {
    protected static final Logger logger = Logger.getLogger(QueryEngineProxy.class.getName());

    // tags
    public static final String
            TAG_RDF_DATA = "rdf-data",
            TAG_SPARQL_QUERY = "sparql-query",
            TAG_SPARQL_RESULT = "sparql-result";

    // fields
    public static final String
            BINDINGS = "bindings",
            DATASET = "dataset",
            QUERY = "query",
            QUERY_ID = "id",
            TTL = "ttl";

    private final Connection connection;
    private final SimpleJSONRDFFormat jsonrdfFormat;

    private final Map<String, Query> queriesById;
    private final Map<String, BindingSetHandler> handlers;

    public QueryEngineProxy(final Connection connection) {
        this.connection = connection;

        jsonrdfFormat = new SimpleJSONRDFFormat(new ValueFactoryImpl());

        queriesById = new HashMap<String, Query>();
        handlers = new HashMap<String, BindingSetHandler>();

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

    public Subscription addQuery(final int ttl,
                                 final String queryStr,
                                 final BindingSetHandler handler)
            throws IncompatibleQueryException, InvalidQueryException, IOException {

        Subscription sub = new SubscriptionImpl();

        Query q = new Query();
        q.queryStr = queryStr;
        q.ttl = ttl;
        queriesById.put(sub.getId(), q);

        if (connection.isActive()) {
            sendSubscriptionMessage(queryStr, sub.getId(), ttl);
        }

        handlers.put(sub.getId(), handler);

        return sub;
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
    public void setClock(QueryEngine.Clock clock) {
        throw new UnsupportedOperationException("sorry, clock cannot be set by proxy");
    }

    @Override
    public void setCleanupPolicy(QueryEngine.CleanupPolicy cleanupPolicy) {
        throw new UnsupportedOperationException("sorry, cleanup policy cannot be set by proxy");
    }

    private void handleSparqlResultJSON(final JSONObject result) throws SimpleJSONRDFFormat.ParseError {
        try {
            String queryId = result.getString(QUERY_ID);

            BindingSetHandler handler = handlers.get(queryId);

            if (null != handler) {
                JSONObject bindings = result.getJSONObject(BINDINGS);

                BindingSet bs = jsonrdfFormat.toBindingSet(bindings);

                // note: no need to catch runtime exceptions here; the connection will survive them
                handler.handle(bs);
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

    private class SubscriptionImpl implements Subscription {
        private final String queryId;
        private boolean active = true;

        public SubscriptionImpl() {
            // Co-opt SmSn atom IDs as query IDs; it's just a convenient way of getting a short pseudo-random string
            // Query IDs are pseudo-random rather than consecutive so as to minimize the chance of collision.
            queryId = SemanticSynchrony.createRandomKey();
        }

        @Override
        public String getId() {
            return queryId;
        }

        @Override
        public boolean isActive() {
            return active;
        }

        @Override
        public void cancel() {
            // TODO
            throw new UnsupportedOperationException("not yet possible to cancel subscriptions through the proxy");
        }

        @Override
        public boolean renew(int ttl) {
            // TODO
            throw new UnsupportedOperationException("not yet possible to renew subscriptions through the proxy");
        }
    }

    private static class Query {
        public String queryStr;
        public long ttl;
    }
}
