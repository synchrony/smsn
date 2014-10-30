package net.fortytwo.extendo.p2p;

import edu.rpi.twc.sesamestream.QueryEngine;
import net.fortytwo.extendo.Extendo;
import net.fortytwo.extendo.p2p.sparql.QueryEngineProxy;
import net.fortytwo.rdfagents.data.DatasetFactory;
import org.openrdf.model.URI;
import org.openrdf.model.ValueFactory;

import java.io.IOException;
import java.net.InetAddress;
import java.net.Socket;
import java.util.logging.Logger;

/**
 * @author Joshua Shinavier (http://fortytwo.net)
 */
public class ExtendoAgent {
    protected static final Logger LOGGER = Logger.getLogger(ExtendoAgent.class.getName());

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

    public ExtendoAgent(final String agentUri,
                        final boolean listenForServices) {
        LOGGER.info("creating Extendo agent with URI " + agentUri);

        this.agentUri = vf.createURI(agentUri);

        facilitatorConnection = new Connection();

        pinger = new Pinger(facilitatorConnection);

        queryEngine = new QueryEngineProxy(facilitatorConnection);

        if (listenForServices) {
            listener = new ServiceBroadcastListener(new ServiceBroadcastListener.EventHandler() {
                public void receivedServiceDescription(InetAddress address, ServiceDescription description) {
                    if (Extendo.VERBOSE) {
                        LOGGER.info("received broadcast message from " + address.getHostAddress()
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
                            LOGGER.info("opening socket connection to facilitator");
                            socket = new Socket(address, facilitatorService.description.getPubsubPort());
                        } catch (IOException e) {
                            LOGGER.severe("failed to open socket connection to facilitator: " + e.getMessage());
                            e.printStackTrace(System.err);
                            return;
                        }

                        try {
                            queryEngine.notifyConnectionOpen();
                        } catch (IOException e) {
                            LOGGER.warning("error on query engine notification: " + e.getMessage());
                            e.printStackTrace(System.err);
                            return;
                        }

                        facilitatorConnection.start(socket);
                    } else {
                        if (Extendo.VERBOSE) {
                            LOGGER.info("ignoring broadcast message due to existing connection to "
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

    public class Service {
        public InetAddress address;
        public ServiceDescription description;
    }
}
