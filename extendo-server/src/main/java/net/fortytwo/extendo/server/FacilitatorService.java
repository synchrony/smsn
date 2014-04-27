package net.fortytwo.extendo.server;

import edu.rpi.twc.sesamestream.QueryEngine;
import edu.rpi.twc.sesamestream.SesameStream;
import edu.rpi.twc.sesamestream.impl.QueryEngineImpl;
import net.fortytwo.extendo.Extendo;
import net.fortytwo.extendo.p2p.Connection;
import net.fortytwo.extendo.p2p.ConnectionHost;
import net.fortytwo.extendo.p2p.PingAnswerer;
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

    private static final String BASE_URI = "http://example.org/baseURI";

    // TODO
    private final String broadcastEndpoint = "/graphs/joshkb/extendo/";

    private final QueryEngine queryEngine;

    public static FacilitatorService getInstance() throws IOException, PropertyException {
        if (null == INSTANCE) {
            INSTANCE = new FacilitatorService();
        }

        return INSTANCE;
    }

    private FacilitatorService() throws IOException, PropertyException {
        int oscPort = Extendo.getConfiguration().getInt(Extendo.P2P_OSC_PORT);
        int pubsubPort = Extendo.getConfiguration().getInt(Extendo.P2P_PUBSUB_PORT);

        if (Extendo.VERBOSE) {
            SesameStream.setDoPerformanceMetrics(true);
            SesameStream.setDoUseCompactLogFormat(false);
        }

        queryEngine = new QueryEngineImpl();
        QueryEngineWrapper wrapper = new QueryEngineWrapper(queryEngine);

        ConnectionHost ch = new ConnectionHost(pubsubPort);
        ch.addNotifier(wrapper.getNotifier());
        ch.addNotifier(new ConnectionHost.Notifier() {
            public void connectionCreated(final Connection c) {
                // add a ping answerer to each new connection
                new PingAnswerer(c);
            }
        });
        ch.start();

        // begin advertising the service now that the query engine is available
        ServiceDescription d = new ServiceDescription(Extendo.getConfiguration().getProperty(Extendo.VERSION),
                broadcastEndpoint,
                oscPort,
                pubsubPort);
        // TODO: stop the broadcaster when this object is destroyed
        new ServiceBroadcaster(d).start();
    }

    public void pushUpdate(final String rdfData,
                           final RDFFormat format) throws IOException {
        long count;

        InputStream in = new ByteArrayInputStream(rdfData.getBytes());
        try {
            count = parseRdfContent(in, format);
            //ds = dsFactory.parse(in, lang);
        } catch (RDFHandlerException e) {
            throw new IOException(e);
        } catch (RDFParseException e) {
            throw new IOException(e);
        } finally {
            in.close();
        }

        if (Extendo.VERBOSE) {
            LOGGER.info("received a dataset with " + count + " statements");
        }
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
}

