package net.fortytwo.smsn.p2p;

import com.google.common.base.Preconditions;
import com.illposed.osc.OSCMessage;
import net.fortytwo.smsn.SemanticSynchrony;
import net.fortytwo.smsn.rdf.RDF4JUtil;
import net.fortytwo.smsn.rdf.RDFDataset;
import org.eclipse.rdf4j.model.IRI;
import org.eclipse.rdf4j.model.Statement;
import org.eclipse.rdf4j.model.ValueFactory;

import java.io.IOException;
import java.net.DatagramPacket;
import java.net.DatagramSocket;
import java.net.InetAddress;
import java.net.Socket;
import java.util.Collection;
import java.util.logging.Level;
import java.util.logging.Logger;

/**
 * P2P agent for SmSn communication.
 *
 * NOTE: SPARQL streaming functionality was removed in Dec 2024 during RDF4J migration.
 * The stream42-sparql library used deprecated OpenRDF Sesame APIs.
 * To restore streaming, consider:
 * - Apache Kafka Streams for event processing
 * - gRPC or WebSockets for real-time communication
 * - RDF4J's native streaming capabilities
 */
public class SmSnAgent {
    private static final Logger logger = Logger.getLogger(SmSnAgent.class.getName());

    public static final String
            PROP_BODY = "body",
            PROP_TAG = "tag";

    private final IRI agentIri;
    private final ValueFactory valueFactory = RDF4JUtil.getValueFactory();

    private ServiceBroadcastListener listener;

    private Service coordinatorService;
    private final Connection coordinatorConnection;

    private final Pinger pinger;

    private DatagramSocket coordinatorOscSocket;
    private InetAddress coordinatorOscAddress;
    private int coordinatorOscPort;

    public SmSnAgent(final boolean listenForServices) {
        this(SemanticSynchrony.getConfiguration().getServices().getAgentIri(), listenForServices);
    }

    public SmSnAgent(final String agentIri,
                     final boolean listenForServices) {
        Preconditions.checkNotNull(agentIri);
        logger.log(Level.INFO, "creating SmSn agent with IRI " + agentIri);

        this.agentIri = valueFactory.createIRI(agentIri);

        coordinatorConnection = new Connection();

        pinger = new Pinger(coordinatorConnection);

        if (listenForServices) {
            listener = new ServiceBroadcastListener((address, description) -> {
                if (SemanticSynchrony.getConfiguration().isVerbose()) {
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

                    coordinatorConnection.start(socket);
                } else {
                    if (SemanticSynchrony.getConfiguration().isVerbose()) {
                        logger.log(Level.FINE, "ignoring broadcast message due to existing connection to "
                                + coordinatorService.address.getHostAddress());
                    }
                }
            });
            listener.start();
        }
    }

    public IRI getAgentIri() {
        return agentIri;
    }

    public Pinger getPinger() {
        return pinger;
    }

    public ValueFactory getValueFactory() {
        return valueFactory;
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
     * Sends an RDF Dataset to the continuous query engine.
     *
     * NOTE: This method is currently a no-op. SPARQL streaming was removed in Dec 2024.
     * To restore this functionality, implement a new streaming backend.
     *
     * @param d   the RDF Dataset to send
     * @param ttl the time-to-live of the data, in milliseconds. Use ttl=0 for an infinite lifetime
     */
    public void sendDataset(final RDFDataset d, final int ttl) {
        // STUB: Streaming functionality removed. See class javadoc for restoration notes.
        logger.log(Level.FINE, "sendDataset called but streaming is not implemented. "
                + "Dataset has " + d.size() + " statements, ttl=" + ttl);
    }

    public class Service {
        public InetAddress address;
        public ServiceDescription description;
    }
}
