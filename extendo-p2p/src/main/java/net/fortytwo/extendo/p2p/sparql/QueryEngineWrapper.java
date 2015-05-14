package net.fortytwo.extendo.p2p.sparql;

import edu.rpi.twc.rdfstream4j.BindingSetHandler;
import edu.rpi.twc.rdfstream4j.QueryEngine;
import edu.rpi.twc.rdfstream4j.Subscription;
import net.fortytwo.extendo.Extendo;
import net.fortytwo.extendo.p2p.Connection;
import net.fortytwo.extendo.p2p.ConnectionHost;
import net.fortytwo.extendo.p2p.MessageHandler;
import org.json.JSONArray;
import org.json.JSONException;
import org.json.JSONObject;
import org.openrdf.model.Statement;
import org.openrdf.model.impl.ValueFactoryImpl;
import org.openrdf.query.BindingSet;

import java.io.IOException;
import java.util.HashMap;
import java.util.Map;
import java.util.logging.Level;
import java.util.logging.Logger;

/**
 * @author Joshua Shinavier (http://fortytwo.net)
 */
public class QueryEngineWrapper {
    protected static final Logger logger = Logger.getLogger(QueryEngineWrapper.class.getName());

    private final QueryEngine queryEngine;

    // currently, there is a single handler for all incoming RDF data, regardless of source
    private final MessageHandler datasetHandler;

    private final SimpleJSONRDFFormat jsonrdfFormat;

    private final Map<String, Connection> connectionsByQueryId;

    private final ConnectionHost.Notifier notifier;

    public QueryEngineWrapper(final QueryEngine queryEngine) {
        this.queryEngine = queryEngine;

        jsonrdfFormat = new SimpleJSONRDFFormat(new ValueFactoryImpl());

        connectionsByQueryId = new HashMap<String, Connection>();

        datasetHandler = new MessageHandler() {
            public void handle(final JSONObject message) {
                try {
                    handleDatasetMessage(message);
                } catch (SimpleJSONRDFFormat.ParseError e) {
                    logger.log(Level.WARNING, "invalid dataset message: " + message, e);
                } catch (IOException e) {
                    // TODO: propagate the QueryEngine's exception back to the proxy
                    logger.log(Level.WARNING, "error raised by query engine", e);
                }
            }
        };

        notifier = new ConnectionHost.Notifier() {
            public void connectionCreated(final Connection c) {
                QueryEngineWrapper.this.newConnection(c);
            }
        };
    }

    public ConnectionHost.Notifier getNotifier() {
        return notifier;
    }

    private void newConnection(final Connection c) {
        c.registerHandler(QueryEngineProxy.TAG_SPARQL_QUERY, new MessageHandler() {
            public void handle(final JSONObject message) {
                if (Extendo.VERBOSE) {
                    logger.info("received query message from "
                            + c.getSocket().getRemoteSocketAddress() + ": " + message);
                }

                try {
                    handleQueryMessage(c, message);
                } catch (QueryEngine.InvalidQueryException e) {
                    // TODO: propagate the QueryEngine's exception back to the proxy
                    logger.log(Level.WARNING, "error raised by query engine", e);
                } catch (IOException e) {
                    // TODO: propagate the QueryEngine's exception back to the proxy
                    logger.log(Level.WARNING, "error raised by query engine", e);
                } catch (QueryEngine.IncompatibleQueryException e) {
                    // TODO: propagate the QueryEngine's exception back to the proxy
                    logger.log(Level.WARNING, "error raised by query engine", e);
                }
            }
        });

        c.registerHandler(QueryEngineProxy.TAG_RDF_DATA, datasetHandler);
    }

    private void handleQueryMessage(final Connection c,
                                    final JSONObject message)
            throws QueryEngine.InvalidQueryException, IOException, QueryEngine.IncompatibleQueryException {
        if (!c.isActive()) {
            logger.severe("can't handle query message; connection is not active");
            return;
        }

        final String queryId;
        String query;
        int ttl;

        try {
            queryId = message.getString(QueryEngineProxy.QUERY_ID);
            query = message.getString(QueryEngineProxy.QUERY);
            ttl = message.getInt(QueryEngineProxy.TTL);
        } catch (JSONException e) {
            logger.log(Level.WARNING, "invalid query message: " + message, e);
            return;
        }

        if (connectionsByQueryId.keySet().contains(queryId)) {
            logger.warning("ignoring query with duplicate id '" + queryId + "'");
            return;
        }

        connectionsByQueryId.put(queryId, c);

        // note: subscription is currently discarded; we never cancel query subscriptions
        Subscription s = queryEngine.addQuery(ttl, query, new BindingSetHandler() {
            public void handle(final BindingSet bs) {
                try {
                    JSONObject bindings = jsonrdfFormat.toJSON(bs);
                    sendQueryResultMessage(c, queryId, bindings);
                } catch (JSONException e) {
                    logger.log(Level.SEVERE, "error in creating query result JSON message", e);
                } catch (IOException e) {
                    logger.log(Level.WARNING, "failed to send query result due to I/O error",  e);
                }
            }
        });
    }

    private void sendQueryResultMessage(final Connection c,
                                        final String queryId,
                                        final JSONObject bindings) throws JSONException, IOException {
        if (!c.isActive()) {
            logger.severe("can't send query result; connection is not active");
            return;
        }

        JSONObject j = new JSONObject();
        j.put(QueryEngineProxy.QUERY_ID, queryId);
        j.put(QueryEngineProxy.BINDINGS, bindings);

        if (Extendo.VERBOSE) {
            logger.info("sending query result message to " + c.getSocket().getRemoteSocketAddress() + ": " + j);
        }

        // send SPARQL results immediately or not at all; don't buffer
        c.sendNow(QueryEngineProxy.TAG_SPARQL_RESULT, j);
    }

    private void handleDatasetMessage(final JSONObject message) throws SimpleJSONRDFFormat.ParseError, IOException {
        if (Extendo.VERBOSE) {
            logger.info("received dataset message: " + message);
        }

        try {
            JSONArray dataset = message.getJSONArray(QueryEngineProxy.DATASET);
            int ttl = message.getInt(QueryEngineProxy.TTL);
            int length = dataset.length();
            Statement[] a = new Statement[length];
            for (int i = 0; i < length; i++) {
                a[i] = jsonrdfFormat.toStatement(dataset.getJSONArray(i));;
            }
            // add statements as an array, which can be slightly more efficient than adding them individually
            queryEngine.addStatements(ttl, a);
        } catch (JSONException e) {
            throw new SimpleJSONRDFFormat.ParseError(e);
        }
    }
}
