package net.fortytwo.extendo.p2p.sparql;

import edu.rpi.twc.sesamestream.BindingSetHandler;
import edu.rpi.twc.sesamestream.QueryEngine;
import edu.rpi.twc.sesamestream.Subscription;
import net.fortytwo.extendo.p2p.Connection;
import org.json.JSONArray;
import org.json.JSONException;
import org.json.JSONObject;
import org.openrdf.model.Statement;
import org.openrdf.model.impl.ValueFactoryImpl;
import org.openrdf.query.BindingSet;

import java.io.IOException;
import java.util.Collection;
import java.util.HashMap;
import java.util.Map;
import java.util.logging.Logger;

/**
 * @author Joshua Shinavier (http://fortytwo.net)
 */
public class QueryEngineProxy implements QueryEngine {
    protected static final Logger LOGGER = Logger.getLogger(QueryEngineProxy.class.getName());

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
            QUERY_ID = "id";


    private final Connection connection;
    private final SimpleJSONRDFFormat jsonrdfFormat;

    private final Map<String, BindingSetHandler> handlers;

    public QueryEngineProxy(final Connection connection) {
        this.connection = connection;

        jsonrdfFormat = new SimpleJSONRDFFormat(new ValueFactoryImpl());

        handlers = new HashMap<String, BindingSetHandler>();

        connection.registerHandler(TAG_SPARQL_RESULT, new Connection.JSONHandler() {
            public void handle(final JSONObject result) {
                try {
                    handleSparqlResultJSON(result);
                } catch (SimpleJSONRDFFormat.ParseError e) {
                    LOGGER.warning("invalid SPARQL query result: " + result);
                    e.printStackTrace(System.err);
                }
            }
        });
    }

    public void clear() {
        throw new UnsupportedOperationException("don't have rights to clear remote query engine");
    }

    public Subscription addQuery(final String query,
                                 final BindingSetHandler handler) throws IncompatibleQueryException, InvalidQueryException, IOException {
        Subscription sub = new SubscriptionImpl();

        try {
            sendSubscriptionMessage(query, sub.getId());
        } catch (JSONException e) {
            throw new IOException(e);
        }

        handlers.put(sub.getId(), handler);

        return sub;
    }

    public void addStatement(Statement statement) throws IOException {
        addStatements(statement);
    }

    public void addStatements(Statement... statements) throws IOException {
        try {
            JSONArray a = jsonrdfFormat.statementsToJSON(statements);

            sendDatasetMessage(a);
        } catch (JSONException e) {
            throw new IOException(e);
        }
    }

    public void addStatements(Collection<Statement> statements) throws IOException {
        try {
            JSONArray a = jsonrdfFormat.statementsToJSON(statements);

            sendDatasetMessage(a);
        } catch (JSONException e) {
            throw new IOException(e);
        }
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
                                         final String queryId) throws JSONException, IOException {
        JSONObject j = new JSONObject();
        j.put(QUERY_ID, queryId);
        j.put(QUERY, query);

        // queries are of central importance and should be buffered to ensure that they are received
        connection.sendBuffered(TAG_SPARQL_QUERY, j);
    }

    private void sendDatasetMessage(final JSONArray statements) throws JSONException, IOException {
        JSONObject j = new JSONObject();
        j.put(DATASET, statements);

        // send RDF data immediately or not at all; don't buffer
        connection.sendNow(TAG_RDF_DATA, j);
    }

    private static long maxQueryId = 0;

    private class SubscriptionImpl implements Subscription {
        private final String queryId;
        private boolean active = true;

        public SubscriptionImpl() {
            queryId = "" + ++maxQueryId;
        }

        public String getId() {
            return queryId;
        }

        public boolean isActive() {
            return active;
        }

        public void cancel() {
            // TODO
            throw new UnsupportedOperationException("not yet possible to cancel subscriptions through the proxy");
        }
    }

}
