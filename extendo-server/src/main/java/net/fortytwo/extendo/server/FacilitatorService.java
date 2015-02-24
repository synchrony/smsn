package net.fortytwo.extendo.server;

import edu.rpi.twc.sesamestream.SesameStream;
import edu.rpi.twc.sesamestream.impl.QueryEngineImpl;
import net.fortytwo.extendo.Extendo;
import net.fortytwo.extendo.p2p.Connection;
import net.fortytwo.extendo.p2p.ConnectionHost;
import net.fortytwo.extendo.p2p.PingAnswerer;
import net.fortytwo.extendo.p2p.ServiceBroadcaster;
import net.fortytwo.extendo.p2p.ServiceDescription;
import net.fortytwo.extendo.p2p.sparql.QueryEngineWrapper;
import net.fortytwo.extendo.server.gesture.GesturalServer;
import net.fortytwo.extendo.util.TypedProperties;
import net.fortytwo.flow.NullSink;
import net.fortytwo.flow.Sink;
import net.fortytwo.flow.rdf.RDFSink;
import net.fortytwo.linkeddata.LinkedDataCache;
import net.fortytwo.rdfagents.model.Dataset;
import net.fortytwo.ripple.RippleException;
import org.openrdf.model.Namespace;
import org.openrdf.model.Statement;
import org.openrdf.rio.ParserConfig;
import org.openrdf.rio.RDFFormat;
import org.openrdf.rio.RDFHandler;
import org.openrdf.rio.RDFHandlerException;
import org.openrdf.rio.RDFParseException;
import org.openrdf.rio.RDFParser;
import org.openrdf.rio.Rio;
import org.openrdf.sail.SailConnection;
import org.openrdf.sail.SailException;
import org.openrdf.sail.memory.MemoryStore;

import java.io.ByteArrayInputStream;
import java.io.IOException;
import java.io.InputStream;
import java.util.Collection;
import java.util.logging.Logger;

/**
 * @author Joshua Shinavier (http://fortytwo.net)
 */
public class FacilitatorService {
    protected static final Logger logger = Logger.getLogger(FacilitatorService.class.getName());

    private static final int
            LINKED_DATA_TTL = 0,
            PUSHED_DATA_TTL = 0;

    private static FacilitatorService INSTANCE;

    private static final String BASE_URI = "http://example.org/baseURI";

    // TODO
    private final String broadcastEndpoint = "/graphs/joshkb/extendo/";

    private final QueryEngineImpl queryEngine;

    public static FacilitatorService getInstance()
            throws IOException, TypedProperties.PropertyException, RippleException, SailException {

        if (null == INSTANCE) {
            INSTANCE = new FacilitatorService();
        }

        return INSTANCE;
    }

    private FacilitatorService()
            throws IOException, TypedProperties.PropertyException, RippleException, SailException {

        int oscPort = Extendo.getConfiguration().getInt(Extendo.P2P_OSC_PORT);
        int pubsubPort = Extendo.getConfiguration().getInt(Extendo.P2P_PUBSUB_PORT);

        if (Extendo.VERBOSE) {
            SesameStream.setDoPerformanceMetrics(true);
            SesameStream.setDoUseCompactLogFormat(false);
        }

        queryEngine = new QueryEngineImpl();
        QueryEngineWrapper wrapper = new QueryEngineWrapper(queryEngine);

        // TODO: make the base Sail configurable (e.g. disable it, or make it a persistent NativeStore)
        MemoryStore sail = new MemoryStore();
        sail.initialize();
        LinkedDataCache.DataStore store = new LinkedDataCache.DataStore() {
            public RDFSink createInputSink(final SailConnection sc) {
                return new RDFSink() {
                    public Sink<Statement> statementSink() {
                        return new Sink<Statement>() {
                            public void put(final Statement s) throws RippleException {
                                queryEngine.addStatements(LINKED_DATA_TTL, s);
                            }
                        };
                    }

                    public Sink<Namespace> namespaceSink() {
                        return new NullSink<Namespace>();
                    }

                    public Sink<String> commentSink() {
                        return new NullSink<String>();
                    }
                };
            }
        };
        LinkedDataCache cache = LinkedDataCache.createDefault(sail);
        cache.setDataStore(store);
        queryEngine.setLinkedDataCache(cache, sail);

        GesturalServer.DatasetHandler h = new GesturalServer.DatasetHandler() {
            @Override
            public void handle(Dataset dataset) {
                System.out.println("received " + dataset.getStatements().size() + " statements from gestural server");
                queryEngine.addStatements(Extendo.GESTURE_TTL, toArray(dataset));
            }
        };

        // gestural event processing via OSC
        GesturalServer gesturalServer = new GesturalServer(oscPort, h);
        gesturalServer.start();

        // SPARQL pub/sub via Extendo P2P
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
            logger.info("received a dataset with " + count + " statements");
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

            queryEngine.addStatements(PUSHED_DATA_TTL, statement);
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

