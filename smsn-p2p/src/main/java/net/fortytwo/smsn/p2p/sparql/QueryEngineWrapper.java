package net.fortytwo.smsn.p2p.sparql;

import net.fortytwo.smsn.SemanticSynchrony;
import net.fortytwo.smsn.p2p.Connection;
import net.fortytwo.smsn.p2p.ConnectionHost;
import net.fortytwo.smsn.p2p.MessageHandler;
import net.fortytwo.stream.StreamProcessor;
import net.fortytwo.stream.sparql.SparqlStreamProcessor;
import org.json.JSONArray;
import org.json.JSONException;
import org.json.JSONObject;
import org.openrdf.model.Statement;
import org.openrdf.model.impl.SimpleValueFactory;
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
    private static final Logger logger = Logger.getLogger(QueryEngineWrapper.class.getName());

    private final SparqlStreamProcessor processor;

    // currently, there is a single handler for all incoming RDF data, regardless of source
    private final MessageHandler datasetHandler;

    private final SimpleJSONRDFFormat jsonrdfFormat;

    private final Map<String, Connection> connectionsByQueryId;

    private final ConnectionHost.Notifier notifier;

    public QueryEngineWrapper(final SparqlStreamProcessor processor) {
        this.processor = processor;

        jsonrdfFormat = new SimpleJSONRDFFormat(SimpleValueFactory.getInstance());

        connectionsByQueryId = new HashMap<>();

        datasetHandler = new MessageHandler() {
            public void handle(final JSONObject message) {
                try {
                    handleDatasetMessage(message);
                } catch (SimpleJSONRDFFormat.ParseError e) {
                    logger.log(Level.WARNING, "invalid dataset message: " + message, e);
                }
            }
        };

        notifier = QueryEngineWrapper.this::newConnection;
    }

    public ConnectionHost.Notifier getNotifier() {
        return notifier;
    }

    private void newConnection(final Connection c) {
        c.registerHandler(ProxySparqlStreamProcessor.TAG_SPARQL_QUERY, message -> {
            if (SemanticSynchrony.VERBOSE) {
                logger.info("received query message from "
                        + c.getSocket().getRemoteSocketAddress() + ": " + message);
            }

            try {
                handleQueryMessage(c, message);
            } catch (StreamProcessor.InvalidQueryException
                    | IOException | StreamProcessor.IncompatibleQueryException e) {
                // TODO: propagate the StreamProcessor's exception back to the proxy
                logger.log(Level.WARNING, "error raised by query engine", e);
            }
        });

        c.registerHandler(ProxySparqlStreamProcessor.TAG_RDF_DATA, datasetHandler);
    }

    private void handleQueryMessage(final Connection c,
                                    final JSONObject message)
            throws StreamProcessor.InvalidQueryException, IOException, StreamProcessor.IncompatibleQueryException {
        if (!c.isActive()) {
            logger.severe("can't handle query message; connection is not active");
            return;
        }

        final String queryId;
        String query;
        int ttl;

        try {
            queryId = message.getString(ProxySparqlStreamProcessor.QUERY_ID);
            query = message.getString(ProxySparqlStreamProcessor.QUERY);
            ttl = message.getInt(ProxySparqlStreamProcessor.TTL);
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
        processor.addQuery(ttl, query, (solution, expirationTime) -> {
            try {
                JSONObject bindings = jsonrdfFormat.toJSON((BindingSet) solution, (Long) expirationTime);
                sendQueryResultMessage(c, queryId, bindings, (Long) expirationTime);
            } catch (JSONException e) {
                logger.log(Level.SEVERE, "error in creating query result JSON message", e);
            } catch (IOException e) {
                logger.log(Level.WARNING, "failed to send query result due to I/O error",  e);
            }
        });
    }

    private void sendQueryResultMessage(final Connection c,
                                        final String queryId,
                                        final JSONObject bindings,
                                        long expirationTime) throws JSONException, IOException {
        if (!c.isActive()) {
            logger.severe("can't send query result; connection is not active");
            return;
        }

        JSONObject j = new JSONObject();
        j.put(ProxySparqlStreamProcessor.QUERY_ID, queryId);
        j.put(ProxySparqlStreamProcessor.MAPPING, bindings);
        j.put(ProxySparqlStreamProcessor.EXPIRATION_TIME, expirationTime);

        if (SemanticSynchrony.VERBOSE) {
            logger.info("sending query result message to " + c.getSocket().getRemoteSocketAddress() + ": " + j);
        }

        // send SPARQL results immediately or not at all; don't buffer
        c.sendNow(ProxySparqlStreamProcessor.TAG_SPARQL_RESULT, j);
    }

    private void handleDatasetMessage(final JSONObject message) throws SimpleJSONRDFFormat.ParseError {
        if (SemanticSynchrony.VERBOSE) {
            logger.info("received dataset message: " + message);
        }

        try {
            JSONArray dataset = message.getJSONArray(ProxySparqlStreamProcessor.DATASET);
            int ttl = message.getInt(ProxySparqlStreamProcessor.TTL);
            int length = dataset.length();
            Statement[] a = new Statement[length];
            for (int i = 0; i < length; i++) {
                a[i] = jsonrdfFormat.toStatement(dataset.getJSONArray(i));
            }
            // add statements as an array, which can be slightly more efficient than adding them individually
            processor.addInputs(ttl, a);
        } catch (JSONException e) {
            throw new SimpleJSONRDFFormat.ParseError(e);
        }
    }
}
