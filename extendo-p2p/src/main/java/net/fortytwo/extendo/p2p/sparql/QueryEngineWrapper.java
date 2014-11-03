package net.fortytwo.extendo.p2p.sparql;

import edu.rpi.twc.sesamestream.BindingSetHandler;
import edu.rpi.twc.sesamestream.QueryEngine;
import edu.rpi.twc.sesamestream.Subscription;
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
                    logger.warning("invalid dataset message: " + message);
                    e.printStackTrace(System.err);
                } catch (IOException e) {
                    // TODO: propagate the QueryEngine's exception back to the proxy
                    logger.warning("error raised by query engine: " + e.getMessage());
                    e.printStackTrace(System.err);
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
                    logger.warning("error raised by query engine: " + e.getMessage());
                    e.printStackTrace(System.err);
                } catch (IOException e) {
                    // TODO: propagate the QueryEngine's exception back to the proxy
                    logger.warning("error raised by query engine: " + e.getMessage());
                    e.printStackTrace(System.err);
                } catch (QueryEngine.IncompatibleQueryException e) {
                    // TODO: propagate the QueryEngine's exception back to the proxy
                    logger.warning("error raised by query engine: " + e.getMessage());
                    e.printStackTrace(System.err);
                }
            }
        });

        c.registerHandler(QueryEngineProxy.TAG_RDF_DATA, datasetHandler);
    }

    private void handleQueryMessage(final Connection c,
                                    final JSONObject message)
            throws QueryEngine.InvalidQueryException, IOException, QueryEngine.IncompatibleQueryException {

        final String queryId;
        String query;

        try {
            queryId = message.getString(QueryEngineProxy.QUERY_ID);
            query = message.getString(QueryEngineProxy.QUERY);
        } catch (JSONException e) {
            logger.warning("invalid query message: " + message);
            e.printStackTrace(System.err);
            return;
        }

        if (connectionsByQueryId.keySet().contains(queryId)) {
            logger.warning("ignoring query with duplicate id '" + queryId + "'");
            return;
        }

        connectionsByQueryId.put(queryId, c);

        // note: subscription is currently discarded; we never cancel query subscriptions
        Subscription s = queryEngine.addQuery(query, new BindingSetHandler() {
            public void handle(final BindingSet bs) {
                try {
                    JSONObject bindings = jsonrdfFormat.toJSON(bs);
                    sendQueryResultMessage(c, queryId, bindings);
                } catch (JSONException e) {
                    logger.severe("error in creating query result JSON message: " + e.getMessage());
                    e.printStackTrace(System.err);
                } catch (IOException e) {
                    logger.warning("failed to send query result due to I/O error: " + e.getMessage());
                    e.printStackTrace(System.err);
                }
            }
        });
    }

    private void sendQueryResultMessage(final Connection c,
                                        final String queryId,
                                        final JSONObject bindings) throws JSONException, IOException {
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
            int length = dataset.length();
            //if (Extendo.VERBOSE) {
            //    logger.info("found " + length + " statements:");
            //}
            for (int i = 0; i < length; i++) {
                Statement s = jsonrdfFormat.toStatement(dataset.getJSONArray(i));
                //if (Extendo.VERBOSE) {
                //    logger.info("\t" + s);
                //}
                queryEngine.addStatement(s);
            }
        } catch (JSONException e) {
            throw new SimpleJSONRDFFormat.ParseError(e);
        }
    }
}
