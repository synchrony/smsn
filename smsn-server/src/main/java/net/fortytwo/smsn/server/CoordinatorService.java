package net.fortytwo.smsn.server;

import net.fortytwo.linkeddata.LinkedDataCache;
import net.fortytwo.rdfagents.model.Dataset;
import net.fortytwo.smsn.SemanticSynchrony;
import net.fortytwo.smsn.p2p.ConnectionHost;
import net.fortytwo.smsn.p2p.PingAnswerer;
import net.fortytwo.smsn.p2p.ServiceBroadcaster;
import net.fortytwo.smsn.p2p.ServiceDescription;
import net.fortytwo.smsn.p2p.sparql.QueryEngineWrapper;
import net.fortytwo.smsn.gesture.GesturalServer;
import net.fortytwo.stream.StreamProcessor;
import net.fortytwo.stream.sparql.SparqlStreamProcessor;
import net.fortytwo.stream.sparql.impl.shj.SHJSparqlStreamProcessor;
import org.openrdf.model.Statement;
import org.openrdf.rio.RDFFormat;
import org.openrdf.rio.RDFHandler;
import org.openrdf.rio.RDFHandlerException;
import org.openrdf.rio.RDFParseException;
import org.openrdf.rio.RDFParser;
import org.openrdf.rio.Rio;
import org.openrdf.rio.helpers.BasicParserSettings;
import org.openrdf.sail.SailException;
import org.openrdf.sail.memory.MemoryStore;

import java.io.ByteArrayInputStream;
import java.io.IOException;
import java.io.InputStream;
import java.util.Collection;
import java.util.function.Consumer;
import java.util.logging.Logger;

public class CoordinatorService {
    private static final Logger logger = Logger.getLogger(CoordinatorService.class.getName());

    private static final int
            LINKED_DATA_TTL = StreamProcessor.INFINITE_TTL,
            PUSHED_DATA_TTL = StreamProcessor.INFINITE_TTL;

    private static CoordinatorService INSTANCE;

    private static final String BASE_IRI = "http://example.org/baseIRI";

    // TODO
    private static final String BROADCAST_ENDPOINT = "/graphs/joshkb/smsn/";

    private final SparqlStreamProcessor streamProcessor;

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

    private CoordinatorService()
            throws IOException, SailException {

        int oscPort = SemanticSynchrony.getConfiguration().getServices().getOsc().getPort();
        int pubsubPort = SemanticSynchrony.getConfiguration().getServices().getPubSub().getPort();

        streamProcessor = new SHJSparqlStreamProcessor();
        QueryEngineWrapper wrapper = new QueryEngineWrapper(streamProcessor);

        if (SemanticSynchrony.getConfiguration().isVerbose()) {
            streamProcessor.setDoPerformanceMetrics(true);
            streamProcessor.setDoUseCompactLogFormat(false);
        }

        // TODO: make the base Sail configurable (e.g. disable it, or make it a persistent NativeStore)
        MemoryStore sail = new MemoryStore();
        sail.initialize();
        LinkedDataCache.DataStore store = sc -> statement -> streamProcessor.addInputs(LINKED_DATA_TTL, statement);
        LinkedDataCache cache = LinkedDataCache.createDefault(sail);
        cache.setDataStore(store);
        streamProcessor.setLinkedDataCache(cache);

        Consumer<Dataset> h = dataset -> {
            System.out.println("received " + dataset.getStatements().size() + " statements from gestural server");
            streamProcessor.addInputs(SemanticSynchrony.GESTURE_TTL, toArray(dataset));
        };

        // gestural event processing via OSC
        GesturalServer gesturalServer = new GesturalServer(oscPort, h);
        gesturalServer.start();

        // SPARQL pub/sub via SmSn Services
        ConnectionHost ch = new ConnectionHost(pubsubPort);
        ch.addNotifier(wrapper.getNotifier());
        ch.addNotifier(PingAnswerer::new);
        ch.start();

        // begin advertising the service now that the query engine is available
        ServiceDescription d = new ServiceDescription(SemanticSynchrony.getConfiguration().getVersion(),
                BROADCAST_ENDPOINT,
                oscPort,
                pubsubPort);
        // TODO: stop the broadcaster when this object is destroyed
        new ServiceBroadcaster(d).start();
    }

    public void pushUpdate(final String rdfData,
                           final RDFFormat format) throws IOException {
        long count;

        try (InputStream in = new ByteArrayInputStream(rdfData.getBytes())) {
            count = parseRdfContent(in, format);
        } catch (RDFHandlerException | RDFParseException e) {
            throw new IOException(e);
        }

        if (SemanticSynchrony.getConfiguration().isVerbose()) {
            logger.info("received a dataset with " + count + " statements");
        }
    }

    // synchronized because the query engine is not thread-safe
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

    private Statement[] toArray(Dataset d) {
        Collection<Statement> c = d.getStatements();
        Statement[] a = new Statement[c.size()];
        return c.toArray(a);
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

            streamProcessor.addInputs(PUSHED_DATA_TTL, statement);
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

