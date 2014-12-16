package net.fortytwo.extendo.p2p;

import com.illposed.osc.OSCMessage;
import edu.rpi.twc.sesamestream.QueryEngine;
import net.fortytwo.extendo.Extendo;
import net.fortytwo.extendo.p2p.sparql.QueryEngineProxy;
import net.fortytwo.rdfagents.data.DatasetFactory;
import net.fortytwo.rdfagents.model.Dataset;
import org.openrdf.model.Statement;
import org.openrdf.model.URI;
import org.openrdf.model.ValueFactory;
import org.openrdf.rio.RDFHandlerException;
import org.openrdf.rio.RDFWriter;
import org.openrdf.rio.ntriples.NTriplesWriter;

import java.io.ByteArrayOutputStream;
import java.io.IOException;
import java.net.DatagramPacket;
import java.net.DatagramSocket;
import java.net.InetAddress;
import java.net.Socket;
import java.util.logging.Level;
import java.util.logging.Logger;

/**
 * @author Joshua Shinavier (http://fortytwo.net)
 */
public class ExtendoAgent {
    protected static final Logger logger = Logger.getLogger(ExtendoAgent.class.getName());

    public static final String
            PROP_BODY = "body",
            PROP_TAG = "tag";

    protected final URI agentUri;
    protected final DatasetFactory factory = new DatasetFactory();
    protected final ValueFactory vf = factory.getValueFactory();

    private ServiceBroadcastListener listener;

    private QueryEngineProxy queryEngine;

    private Service facilitatorService;
    private final Connection facilitatorConnection;

    private final Pinger pinger;

    private DatagramSocket facilitatorOscSocket;
    private InetAddress facilitatorOscAddress;
    private int facilitatorOscPort;

    public ExtendoAgent(final boolean listenForServices) {
        this(Extendo.getConfiguration().getProperty(Extendo.P2P_AGENT_URI), listenForServices);
    }

    public ExtendoAgent(final String agentUri,
                        final boolean listenForServices) {
        logger.log(Level.INFO, "creating Extendo agent with URI " + agentUri);

        this.agentUri = vf.createURI(agentUri);

        facilitatorConnection = new Connection();

        pinger = new Pinger(facilitatorConnection);

        queryEngine = new QueryEngineProxy(facilitatorConnection);

        if (listenForServices) {
            listener = new ServiceBroadcastListener(new ServiceBroadcastListener.EventHandler() {
                public void receivedServiceDescription(InetAddress address, ServiceDescription description) {
                    if (Extendo.VERBOSE) {
                        logger.log(Level.FINE, "received broadcast message from " + address.getHostAddress()
                                + ": version=" + description.getVersion()
                                + ", endpoint=" + description.getEndpoint()
                                + ", pub/sub port=" + description.getPubsubPort());
                    }

                    // The first broadcast message is used to discover the service and create a connection.
                    // Subsequent messages are used only if the connection is lost.
                    if (!facilitatorConnection.isActive()) {
                        facilitatorService = new Service();
                        facilitatorService.address = address;
                        facilitatorService.description = description;

                        Socket socket;
                        try {
                            logger.log(Level.INFO, "opening socket connection to facilitator");
                            socket = new Socket(address, facilitatorService.description.getPubsubPort());
                        } catch (IOException e) {
                            logger.log(Level.INFO, "failed to open socket connection to facilitator", e);
                            return;
                        }

                        try {
                            queryEngine.notifyConnectionOpen();
                        } catch (IOException e) {
                            logger.log(Level.WARNING, "error on query engine notification", e);
                            return;
                        }

                        facilitatorConnection.start(socket);
                    } else {
                        if (Extendo.VERBOSE) {
                            logger.log(Level.FINE, "ignoring broadcast message due to existing connection to "
                                    + facilitatorService.address.getHostAddress());
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

    public Service getFacilitatorService() {
        return facilitatorService;
    }

    public Connection getFacilitatorConnection() {
        return facilitatorConnection;
    }

    public void stop() {
        if (null != listener) {
            listener.stop();
        }
    }

    public void sendOSCMessageToFacilitator(final OSCMessage m) {
        if (getFacilitatorConnection().isActive()) {
            try {
                if (null == facilitatorOscSocket) {
                    facilitatorOscPort = getFacilitatorService().description.getOscPort();
                    facilitatorOscAddress = getFacilitatorService().address;

                    facilitatorOscSocket = new DatagramSocket();
                }

                byte[] buffer = m.getByteArray();
                DatagramPacket packet
                        = new DatagramPacket(buffer, buffer.length, facilitatorOscAddress, facilitatorOscPort);
                facilitatorOscSocket.send(packet);

                logger.log(Level.INFO, "sent OSC datagram to " + facilitatorOscAddress + ":" + facilitatorOscPort);
            } catch (IOException e) {
                logger.log(Level.SEVERE, "error in sending OSC datagram to facilitator", e);
            } catch (Throwable t) {
                logger.log(Level.SEVERE, "unexpected error in sending OSC datagram to facilitator", t);
            }
        }
    }

    public void sendDataset(final Dataset d) throws IOException {
        getQueryEngine().addStatements(d.getStatements());

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
            sendOSCMessageToFacilitator(m);
        }
        */
    }

    public class Service {
        public InetAddress address;
        public ServiceDescription description;
    }
}
