package net.fortytwo.smsn.server;

import net.fortytwo.smsn.SemanticSynchrony;
import net.fortytwo.smsn.p2p.ConnectionHost;
import net.fortytwo.smsn.p2p.PingAnswerer;
import net.fortytwo.smsn.p2p.ServiceBroadcaster;
import net.fortytwo.smsn.p2p.ServiceDescription;
import org.eclipse.rdf4j.model.Statement;
import org.eclipse.rdf4j.rio.RDFFormat;
import org.eclipse.rdf4j.rio.RDFHandler;
import org.eclipse.rdf4j.rio.RDFHandlerException;
import org.eclipse.rdf4j.rio.RDFParseException;
import org.eclipse.rdf4j.rio.RDFParser;
import org.eclipse.rdf4j.rio.Rio;
import org.eclipse.rdf4j.rio.helpers.BasicParserSettings;

import java.io.ByteArrayInputStream;
import java.io.IOException;
import java.io.InputStream;
import java.util.logging.Logger;

/**
 * Coordinator service for P2P SmSn communication.
 *
 * NOTE: SPARQL streaming and gestural server functionality was removed in Dec 2024
 * during RDF4J migration. The following features are no longer available:
 * - Continuous SPARQL query processing (stream42-sparql)
 * - Linked data caching with streaming
 * - Gestural event processing integration
 *
 * To restore these features, consider:
 * - Apache Kafka Streams for event processing
 * - RDF4J's native capabilities
 * - gRPC or WebSockets for real-time communication
 */
public class CoordinatorService {
    private static final Logger logger = Logger.getLogger(CoordinatorService.class.getName());

    private static final int
            LINKED_DATA_TTL = 0,  // Was StreamProcessor.INFINITE_TTL
            PUSHED_DATA_TTL = 0;  // Was StreamProcessor.INFINITE_TTL

    private static CoordinatorService INSTANCE;

    private static final String BASE_IRI = "http://example.org/baseIRI";

    // TODO
    private static final String BROADCAST_ENDPOINT = "/graphs/joshkb/smsn/";

    public static CoordinatorService getInstance() {
        if (null == INSTANCE) {
            try {
                INSTANCE = new CoordinatorService();
            } catch (IOException e) {
                throw new IllegalStateException(e);
            }
        }
        return INSTANCE;
    }

    private CoordinatorService() throws IOException {
        int oscPort = SemanticSynchrony.getConfiguration().getServices().getOsc().getPort();
        int pubsubPort = SemanticSynchrony.getConfiguration().getServices().getPubSub().getPort();

        // NOTE: SPARQL stream processor removed. Was: new SHJSparqlStreamProcessor()
        // NOTE: LinkedDataCache integration removed.
        // NOTE: GesturalServer integration removed.

        logger.warning("CoordinatorService initialized without streaming support. "
                + "SPARQL streaming was removed in Dec 2024 during RDF4J migration.");

        // SPARQL pub/sub via SmSn Services (connection infrastructure still works)
        ConnectionHost ch = new ConnectionHost(pubsubPort);
        ch.addNotifier(PingAnswerer::new);
        ch.start();

        // begin advertising the service
        ServiceDescription d = new ServiceDescription(SemanticSynchrony.getConfiguration().getVersion(),
                BROADCAST_ENDPOINT,
                oscPort,
                pubsubPort);
        new ServiceBroadcaster(d).start();
    }

    /**
     * Push RDF update data.
     *
     * NOTE: This method parses the RDF but does not process it through a stream processor
     * since SPARQL streaming was removed.
     *
     * @param rdfData the RDF data as a string
     * @param format  the RDF format
     * @throws IOException if parsing fails
     */
    public void pushUpdate(final String rdfData,
                           final RDFFormat format) throws IOException {
        long count;

        try (InputStream in = new ByteArrayInputStream(rdfData.getBytes())) {
            count = parseRdfContent(in, format);
        } catch (RDFHandlerException | RDFParseException e) {
            throw new IOException(e);
        }

        if (SemanticSynchrony.getConfiguration().isVerbose()) {
            logger.info("received a dataset with " + count + " statements (not processed - streaming removed)");
        }
    }

    private long parseRdfContent(final InputStream content,
                                 final RDFFormat format) throws RDFParseException, IOException, RDFHandlerException {

        RDFParser parser = Rio.createParser(format);
        parser.getParserConfig().set(BasicParserSettings.VERIFY_DATATYPE_VALUES, false);

        ParsedRDFHandler parsedRDFHandler = new ParsedRDFHandler();

        parsedRDFHandler.clear();
        parser.setRDFHandler(parsedRDFHandler);
        parser.parse(content, BASE_IRI);
        return parsedRDFHandler.getCount();
    }

    private class ParsedRDFHandler implements RDFHandler {
        private long count = 0;

        public void startRDF() throws RDFHandlerException {
            logger.fine("beginning of RDF document");
        }

        public void endRDF() throws RDFHandlerException {
            logger.fine("end of RDF document");
        }

        public void handleNamespace(String s, String s2) throws RDFHandlerException {
        }

        public void handleStatement(Statement statement) throws RDFHandlerException {
            count++;
            // NOTE: Was: streamProcessor.addInputs(PUSHED_DATA_TTL, statement);
            // Streaming removed - statement is counted but not processed
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
