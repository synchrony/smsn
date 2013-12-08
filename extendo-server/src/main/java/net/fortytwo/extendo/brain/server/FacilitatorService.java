package net.fortytwo.extendo.brain.server;

import edu.rpi.twc.sesamestream.BindingSetHandler;
import edu.rpi.twc.sesamestream.Query;
import edu.rpi.twc.sesamestream.QueryEngine;
import net.fortytwo.extendo.Extendo;
import net.fortytwo.extendo.p2p.ServiceBroadcaster;
import net.fortytwo.extendo.p2p.ServiceDescription;
import net.fortytwo.extendo.rdf.vocab.ExtendoGesture;
import net.fortytwo.extendo.rdf.vocab.Timeline;
import net.fortytwo.extendo.util.properties.PropertyException;
import net.fortytwo.rdfagents.data.DatasetFactory;
import net.fortytwo.rdfagents.model.RDFContentLanguage;
import org.openrdf.model.Statement;
import org.openrdf.query.Binding;
import org.openrdf.query.BindingSet;
import org.openrdf.query.MalformedQueryException;
import org.openrdf.query.QueryLanguage;
import org.openrdf.query.algebra.TupleExpr;
import org.openrdf.query.parser.ParsedQuery;
import org.openrdf.query.parser.QueryParserUtil;
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
import java.io.OutputStream;
import java.net.ServerSocket;
import java.net.Socket;
import java.util.logging.Logger;

/**
 * @author Joshua Shinavier (http://fortytwo.net)
 */
public class FacilitatorService {
    protected static final Logger LOGGER = Logger.getLogger(FacilitatorService.class.getName());

    private static FacilitatorService INSTANCE;

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

    // TODO
    private String broadcastEndpoint = "/graphs/joshkb/extendo/";

    private final DatasetFactory dsFactory = new DatasetFactory();
    private RDFContentLanguage lang;

    private OutputStream notificationStream;
    private QueryEngine queryEngine;

    public static FacilitatorService getInstance() throws IOException, PropertyException {
        if (null == INSTANCE) {
            INSTANCE = new FacilitatorService();
        }

        return INSTANCE;
    }

    private FacilitatorService() throws IOException, PropertyException {
        for (RDFContentLanguage l : dsFactory.getSupportedLanguages()) {
            if (l.getFormat().equals(RDFFormat.NTRIPLES)) {
                lang = l;
                break;
            }
        }

        if (null == lang) {
            throw new IllegalStateException("facilitator requires the N-Triples format, but it is not supported as an RDF content language");
        }

        new Thread(new Runnable() {
            public void run() {
                try {
                    startContinuousQueryEngine();

                    // this executes indefinitely in the nested thread, keeping both the socket connection
                    // and the query engine alive
                    manageNotificationStream();
                } catch (Throwable e) {
                    LOGGER.severe("SPARQL notification stream closed with error: " + e.getMessage());
                    e.printStackTrace(System.err);
                }
            }
        }).start();

        int port = Extendo.getConfiguration().getInt(Extendo.P2P_PUBSUB_PORT);

        ServiceDescription d = new ServiceDescription(Extendo.getConfiguration().getProperty(Extendo.VERSION),
                broadcastEndpoint,
                port);
        // TODO: stop the broadcaster when this object is destroyed
        new ServiceBroadcaster(d).start();
    }

    private void startContinuousQueryEngine() throws Query.IncompatibleQueryException, MalformedQueryException {
        // TODO: temporary; in future, queries will be supplied by the client
        TupleExpr singleQuery = parseQuery(QUERY_FOR_ALL_GB_GESTURES);

        queryEngine = new QueryEngine();
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

    private void manageNotificationStream() throws IOException, InterruptedException {

        final int portNumber = 1331;
        LOGGER.info("instantiating facilitator service on port " + portNumber);
        ServerSocket serverSocket = new ServerSocket(portNumber);
        while (true) {
            LOGGER.info("opening SPARQL notification socket for writing");
            Socket socket = serverSocket.accept();
            notificationStream = socket.getOutputStream();
            try {
                //PrintWriter pw = new PrintWriter(notificationStream, true);
                //pw.println("What's you name?");

                // wait indefinitely in this thread
                Object m = "";
                synchronized (m) {
                    m.wait();
                }
                /*
                BufferedReader br = new BufferedReader(new InputStreamReader(socket.getInputStream()));
                String str = br.readLine();

                pw.println("Hello, " + str);
                pw.close();
                socket.close();
                */
            } finally {
                notificationStream = null;
            }
        }
    }

    private TupleExpr parseQuery(final String queryStr) throws MalformedQueryException {
        ParsedQuery q = QueryParserUtil.parseQuery(
                QueryLanguage.SPARQL,
                queryStr,
                "http://example.org/baseURI");
        return q.getTupleExpr();
    }

    public void pushUpdate(final String data) throws IOException {
        long count;

        InputStream in = new ByteArrayInputStream(data.getBytes());
        try {
            LOGGER.info("parsing...");
            count = parseRdfContent(in);
            //ds = dsFactory.parse(in, lang);
        } catch (RDFHandlerException e) {
            throw new IOException(e);
        } catch (RDFParseException e) {
            throw new IOException(e);
        } finally {
            in.close();
        }

        sendNotification("received a dataset with " + count + " statements");
    }

    private static final String BASE_URI = "http://example.org/baseURI";

    // synchronized because the query engine is not thread-safe
    private long parseRdfContent(final InputStream content) throws RDFParseException, IOException, RDFHandlerException {

        RDFParser parser = Rio.createParser(RDFFormat.NTRIPLES);
        ParserConfig config = new ParserConfig(false, false, false, RDFParser.DatatypeHandling.IGNORE);
        parser.setParserConfig(config);

        ParsedRDFHandler parsedRDFHandler = new ParsedRDFHandler();

        parsedRDFHandler.clear();
        parser.setRDFHandler(parsedRDFHandler);
        parser.parse(content, BASE_URI);
        return parsedRDFHandler.getCount();
    }

    private void sendNotification(final String content) throws IOException {
        if (null == notificationStream) {
            LOGGER.severe("can't send notification: stream is null");
        } else {
            notificationStream.write(content.getBytes());
            notificationStream.write('\n');
        }
    }

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

            // TODO: just for now, don't add to the query engine
            //queryEngine.addStatement(statement);
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
}

