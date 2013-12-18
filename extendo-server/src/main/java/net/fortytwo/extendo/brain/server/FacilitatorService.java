package net.fortytwo.extendo.brain.server;

import edu.rpi.twc.sesamestream.QueryEngine;
import edu.rpi.twc.sesamestream.SesameStream;
import edu.rpi.twc.sesamestream.impl.QueryEngineImpl;
import net.fortytwo.extendo.Extendo;
import net.fortytwo.extendo.p2p.ConnectionHost;
import net.fortytwo.extendo.p2p.ServiceBroadcaster;
import net.fortytwo.extendo.p2p.ServiceDescription;
import net.fortytwo.extendo.p2p.sparql.QueryEngineWrapper;
import net.fortytwo.extendo.util.properties.PropertyException;
import org.openrdf.model.Statement;
import org.openrdf.rio.ParserConfig;
import org.openrdf.rio.RDFFormat;
import org.openrdf.rio.RDFHandler;
import org.openrdf.rio.RDFHandlerException;
import org.openrdf.rio.RDFParseException;
import org.openrdf.rio.RDFParser;
import org.openrdf.rio.Rio;

import java.io.ByteArrayInputStream;
import java.io.IOException;
import java.io.InputStream;
import java.util.logging.Logger;

/**
 * @author Joshua Shinavier (http://fortytwo.net)
 */
public class FacilitatorService {
    protected static final Logger LOGGER = Logger.getLogger(FacilitatorService.class.getName());

    private static FacilitatorService INSTANCE;

    /*
    // TODO: copied from BrainstemAgent; deduplicate
    private static final String QUERY_FOR_ALL_GB_GESTURES =
            "PREFIX gesture: <" + ExtendoGesture.NAMESPACE + ">\n" +
                    "PREFIX tl: <" + Timeline.NAMESPACE + ">\n" +
                    "SELECT ?person ?time WHERE {\n" +
                    "?gesture a gesture:GenericBatonGesture .\n" +
                    "?gesture gesture:expressedBy ?person .\n" +
                    "?gesture gesture:recognizedAt ?instant .\n" +
                    "?instant tl:at ?time .\n" +
                    "}";
    */

    private static final String BASE_URI = "http://example.org/baseURI";

    // TODO
    private final String broadcastEndpoint = "/graphs/joshkb/extendo/";
    //private final int pubsubPort;

    //private final DatasetFactory dsFactory = new DatasetFactory();
    //private RDFContentLanguage lang;

    /*
    // TODO: support multiple concurrent connections (one per SesameStream client, with multiple subscriptions per connection)
    private PubSubConnection singleConnection;
    */

    private final QueryEngine queryEngine;

    public static FacilitatorService getInstance() throws IOException, PropertyException {
        if (null == INSTANCE) {
            INSTANCE = new FacilitatorService();
        }

        return INSTANCE;
    }

    private FacilitatorService() throws IOException, PropertyException {
        /*
        for (RDFContentLanguage l : dsFactory.getSupportedLanguages()) {
            if (l.getFormat().equals(RDFFormat.NTRIPLES)) {
                lang = l;
                break;
            }
        }

        if (null == lang) {
            throw new IllegalStateException("facilitator requires the N-Triples format, but it is not supported as an RDF content language");
        }
        */

        /*
        new Thread(new Runnable() {
            public void run() {
                try {
                    startContinuousQueryEngine();

                    // this executes indefinitely in the nested thread, keeping both the socket connection
                    // and the query engine alive
                    managePubSubConnection();
                } catch (Throwable e) {
                    LOGGER.severe("SPARQL notification stream closed with error: " + e.getMessage());
                    e.printStackTrace(System.err);
                }
            }
        }).start();
        */

        int pubsubPort = Extendo.getConfiguration().getInt(Extendo.P2P_PUBSUB_PORT);

        // TODO: temporary, for development and debugging
        SesameStream.setDoPerformanceMetrics(true);
        SesameStream.setDoUseCompactLogFormat(false);

        queryEngine = new QueryEngineImpl();
        QueryEngineWrapper wrapper = new QueryEngineWrapper(queryEngine);

        ConnectionHost ch = new ConnectionHost(pubsubPort);
        ch.addNotifier(wrapper.getNotifier());
        ch.start();

        // begin advertising the service now that the query engine is available
        ServiceDescription d = new ServiceDescription(Extendo.getConfiguration().getProperty(Extendo.VERSION),
                broadcastEndpoint,
                pubsubPort);
        // TODO: stop the broadcaster when this object is destroyed
        new ServiceBroadcaster(d).start();
    }

    /*
    private void startContinuousQueryEngine() throws QueryEngine.IncompatibleQueryException, QueryEngine.InvalidQueryException {
        // TODO: temporary; in future, queries will be supplied by the client
        String singleQuery = QUERY_FOR_ALL_GB_GESTURES;

        queryEngine = new QueryEngineImpl();
        queryEngine.addQuery(singleQuery, new BindingSetHandler() {
            public void handle(final BindingSet bindings) {
                LOGGER.info("found a result for the continuous query");
                boolean first = true;
                StringBuilder sb = new StringBuilder();
                for (Binding b : bindings) {
                    if (first) {
                        first = false;
                    } else {
                        sb.append("\t");
                    }

                    sb.append(b.getName()).append("\t").append(b.getValue().stringValue());
                }

                try {
                    sendNotification(sb.toString());
                } catch (IOException e) {
                    LOGGER.warning("failed to push SPARQL notification: " + e.getMessage());
                }
            }
        });
    }
    */

    /*
    private void managePubSubConnection() throws IOException, InterruptedException, PropertyException {

        LOGGER.info("instantiating facilitator service on port " + pubsubPort);
        ServerSocket serverSocket = new ServerSocket(pubsubPort);
        while (true) {
            LOGGER.info("opening SPARQL pub/sub connection");
            Socket socket = serverSocket.accept();
            singleConnection = new PubSubConnection(socket);
            LOGGER.info("SPARQL pub/sub connection opened to " + socket.getRemoteSocketAddress() + " (" + socket.getInetAddress() + ")");
            try {
                //PrintWriter pw = new PrintWriter(notificationStream, true);
                //pw.println("What's you name?");

                // wait indefinitely in this thread
                Object m = "";
                synchronized (m) {
                    m.wait();
                }
            } finally {
                LOGGER.info("closing SPARQL pub/sub connection");
                singleConnection = null;
                socket.close();
            }
        }
    }*/

    /*
    private TupleExpr parseQuery(final String queryStr) throws MalformedQueryException {
        ParsedQuery q = QueryParserUtil.parseQuery(
                QueryLanguage.SPARQL,
                queryStr,
                "http://example.org/baseURI");
        return q.getTupleExpr();
    }*/

    public void pushUpdate(final String rdfData,
                           final RDFFormat format) throws IOException {
        long count;

        InputStream in = new ByteArrayInputStream(rdfData.getBytes());
        try {
            LOGGER.info("parsing...");
            count = parseRdfContent(in, format);
            //ds = dsFactory.parse(in, lang);
        } catch (RDFHandlerException e) {
            throw new IOException(e);
        } catch (RDFParseException e) {
            throw new IOException(e);
        } finally {
            in.close();
        }

        System.out.println("received a dataset with " + count + " statements");
    }

    // synchronized because the query engine is not thread-safe
    private long parseRdfContent(final InputStream content,
                                 final RDFFormat format) throws RDFParseException, IOException, RDFHandlerException {

        RDFParser parser = Rio.createParser(format);
        ParserConfig config = new ParserConfig(false, false, false, RDFParser.DatatypeHandling.IGNORE);
        parser.setParserConfig(config);

        ParsedRDFHandler parsedRDFHandler = new ParsedRDFHandler();

        parsedRDFHandler.clear();
        parser.setRDFHandler(parsedRDFHandler);
        parser.parse(content, BASE_URI);
        return parsedRDFHandler.getCount();
    }

    /*
    private void sendNotification(final String content) throws IOException {
        if (null == singleConnection) {
            LOGGER.severe("can't send notification: no pub/sub connection");
        } else {
            OutputStream out = singleConnection.getSocket().getOutputStream();
            out.write(content.getBytes());
            out.write('\n');
        }
    }*/

    private class ParsedRDFHandler implements RDFHandler {
        private long count = 0;

        public void startRDF() throws RDFHandlerException {
            LOGGER.fine("beginning of RDF document");
        }

        public void endRDF() throws RDFHandlerException {
            LOGGER.fine("end of RDF document");
        }

        public void handleNamespace(String s, String s2) throws RDFHandlerException {
        }

        public void handleStatement(Statement statement) throws RDFHandlerException {
            count++;

            try {
                queryEngine.addStatement(statement);
            } catch (IOException e) {
                throw new RDFHandlerException(e);
            }
        }

        public void handleComment(String s) throws RDFHandlerException {
        }

        public long getCount() {
            return count;
        }

        public void clear() {
            count = 0;
        }
    }

    /*
    private class PubSubConnection {
        private final Socket socket;

        public PubSubConnection(Socket socket) {
            this.socket = socket;
        }

        public Socket getSocket() {
            return socket;
        }
    }*/
}

