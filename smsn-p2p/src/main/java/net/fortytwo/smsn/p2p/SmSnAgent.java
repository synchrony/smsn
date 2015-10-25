package net.fortytwo.smsn.p2p;

import com.illposed.osc.OSCMessage;
import edu.rpi.twc.sesamestream.QueryEngine;
import net.fortytwo.smsn.SemanticSynchrony;
import net.fortytwo.smsn.p2p.sparql.QueryEngineProxy;
import net.fortytwo.rdfagents.data.DatasetFactory;
import net.fortytwo.rdfagents.model.Dataset;
import org.openrdf.model.Statement;
import org.openrdf.model.URI;
import org.openrdf.model.ValueFactory;

import java.io.IOException;
import java.net.DatagramPacket;
import java.net.DatagramSocket;
import java.net.InetAddress;
import java.net.Socket;
import java.util.Collection;
import java.util.logging.Level;
import java.util.logging.Logger;

/**
 * @author Joshua Shinavier (http://fortytwo.net)
 */
public class SmSnAgent {
    protected static final Logger logger = Logger.getLogger(SmSnAgent.class.getName());

    public static final String
            PROP_BODY = "body",
            PROP_TAG = "tag";

    protected final URI agentUri;
    protected final DatasetFactory factory = new DatasetFactory();
    protected final ValueFactory vf = factory.getValueFactory();

    private ServiceBroadcastListener listener;

    private QueryEngineProxy queryEngine;

    private Service coordinatorService;
    private final Connection coordinatorConnection;

    private final Pinger pinger;

    private DatagramSocket coordinatorOscSocket;
    private InetAddress coordinatorOscAddress;
    private int coordinatorOscPort;

    public SmSnAgent(final boolean listenForServices) {
        this(SemanticSynchrony.getConfiguration().getProperty(SemanticSynchrony.P2P_AGENT_URI), listenForServices);
    }

    public SmSnAgent(final String agentUri,
                     final boolean listenForServices) {
        logger.log(Level.INFO, "creating SmSn agent with URI " + agentUri);

        this.agentUri = vf.createURI(agentUri);

        coordinatorConnection = new Connection();

        pinger = new Pinger(coordinatorConnection);

        queryEngine = new QueryEngineProxy(coordinatorConnection);

        if (listenForServices) {
            listener = new ServiceBroadcastListener(new ServiceBroadcastListener.EventHandler() {
                public void receivedServiceDescription(InetAddress address, ServiceDescription description) {
                    if (SemanticSynchrony.VERBOSE) {
                        logger.log(Level.FINE, "received broadcast message from " + address.getHostAddress()
                                + ": version=" + description.getVersion()
                                + ", endpoint=" + description.getEndpoint()
                                + ", pub/sub port=" + description.getPubsubPort());
                    }

                    // The first broadcast message is used to discover the service and create a connection.
                    // Subsequent messages are used only if the connection is lost.
                    if (!coordinatorConnection.isActive()) {
                        coordinatorService = new Service();
                        coordinatorService.address = address;
                        coordinatorService.description = description;

                        Socket socket;
                        try {
                            logger.log(Level.INFO, "opening socket connection to coordinator");
                            socket = new Socket(address, coordinatorService.description.getPubsubPort());
                        } catch (IOException e) {
                            logger.log(Level.INFO, "failed to open socket connection to coordinator", e);
                            return;
                        }

                        try {
                            queryEngine.notifyConnectionOpen();
                        } catch (IOException e) {
                            logger.log(Level.WARNING, "error on query engine notification", e);
                            return;
                        }

                        coordinatorConnection.start(socket);
                    } else {
                        if (SemanticSynchrony.VERBOSE) {
                            logger.log(Level.FINE, "ignoring broadcast message due to existing connection to "
                                    + coordinatorService.address.getHostAddress());
                        }
                    }
                }
            });
            listener.start();
        }
    }

    public URI getAgentUri() {
        return agentUri;
    }

    public Pinger getPinger() {
        return pinger;
    }

    public QueryEngine getQueryEngine() {
        return queryEngine;
    }

    public DatasetFactory getDatasetFactory() {
        return factory;
    }

    public Service getCoordinatorService() {
        return coordinatorService;
    }

    public Connection getCoordinatorConnection() {
        return coordinatorConnection;
    }

    public void stop() {
        if (null != listener) {
            listener.stop();
        }
    }

    public void sendOSCMessageToCoordinator(final OSCMessage m) {
        if (getCoordinatorConnection().isActive()) {
            try {
                if (null == coordinatorOscSocket) {
                    coordinatorOscPort = getCoordinatorService().description.getOscPort();
                    coordinatorOscAddress = getCoordinatorService().address;

                    coordinatorOscSocket = new DatagramSocket();
                }

                byte[] buffer = m.getByteArray();
                DatagramPacket packet
                        = new DatagramPacket(buffer, buffer.length, coordinatorOscAddress, coordinatorOscPort);
                coordinatorOscSocket.send(packet);

                logger.log(Level.INFO, "sent OSC datagram to " + coordinatorOscAddress + ":" + coordinatorOscPort);
            } catch (IOException e) {
                logger.log(Level.SEVERE, "error in sending OSC datagram to coordinator", e);
            } catch (Throwable t) {
                logger.log(Level.SEVERE, "unexpected error in sending OSC datagram to coordinator", t);
            }
        }
    }

    /**
     * Sends an RDF Dataset to the continuous query engine
     *
     * @param d   the RDF Dataset to send
     * @param ttl the time-to-live of the data, in milliseconds. Use ttl=0 for an infinite lifetime
     * @throws IOException if communication with the query engine fails
     */
    public void sendDataset(final Dataset d, final int ttl) throws IOException {
        getQueryEngine().addStatements(ttl, toArray(d));

        /*
        // TODO: temporary
        OutputStream os = new FileOutputStream("/tmp/smsn.nt");
        RDFWriter w = new NTriplesWriter(os);
        try {
            w.startRDF();
            for (Statement s : d.getStatements()) {
                w.handleStatement(s);
            }
            w.endRDF();
        } catch (RDFHandlerException e) {
            throw new IOException(e);
        }
        os.close();
        */

        /*
        if (relayAsOsc) {
            ByteArrayOutputStream bos = new ByteArrayOutputStream();
            // note: direct instantiation of a format-specific writer (as opposed to classloading via Rio)
            // makes things simpler w.r.t. Proguard's shrinking phase
            RDFWriter w = new NTriplesWriter(bos);
            //RDFWriter w = Rio.createWriter(RDFFormat.NTRIPLES, bos);
            try {
                w.startRDF();
                for (Statement s : d.getStatements()) {
                    w.handleStatement(s);
                }
                w.endRDF();
            } catch (RDFHandlerException e) {
                throw new IOException(e);
            }
            OSCMessage m = new OSCMessage("/exo/fctr/tt/rdf");
            m.addArgument(new String(bos.toByteArray()));
            sendOSCMessageToCoordinator(m);
        }
        */
    }

    private Statement[] toArray(Dataset d) {
        Collection<Statement> c = d.getStatements();
        Statement[] a = new Statement[c.size()];
        return c.toArray(a);
    }

    public class Service {
        public InetAddress address;
        public ServiceDescription description;
    }
}
