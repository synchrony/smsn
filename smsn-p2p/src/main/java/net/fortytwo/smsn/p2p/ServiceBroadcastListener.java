package net.fortytwo.smsn.p2p;

import net.fortytwo.smsn.SemanticSynchrony;
import net.fortytwo.smsn.util.TypedProperties;

import java.io.IOException;
import java.net.DatagramPacket;
import java.net.DatagramSocket;
import java.net.InetAddress;
import java.util.logging.Logger;

/**
 * @author Joshua Shinavier (http://fortytwo.net)
 */
public class ServiceBroadcastListener {
    private static final Logger logger = Logger.getLogger(ServiceBroadcastListener.class.getName());

    private static final int BROADCAST_MAX_LENGTH = 1000;

    private final EventHandler eventHandler;

    private boolean stopped;

    public ServiceBroadcastListener(final EventHandler eventHandler) {
        this.eventHandler = eventHandler;
    }

    public void start() {
        stopped = false;

        new Thread(() -> {
            logger.info("starting listener thread for coordinator broadcast messages");

            try {
                listenForCoordinatorBroadcast();
            } catch (Throwable t) {
                logger.severe("listener thread for coordinator broadcast messages failed with error: "
                        + t.getMessage());
                t.printStackTrace(System.err);
            } finally {
                logger.info("listener thread for coordinator broadcast messages stopped");
            }
        }).start();
    }

    public void stop() {
        stopped = true;
    }

    private void listenForCoordinatorBroadcast() throws TypedProperties.PropertyException {
        int port = SemanticSynchrony.getConfiguration().getInt(SemanticSynchrony.P2P_BROADCAST_PORT);

        byte[] buffer = new byte[BROADCAST_MAX_LENGTH];
        try {
            DatagramPacket dataIn = new DatagramPacket(buffer, buffer.length);
            try (DatagramSocket socket = new DatagramSocket(port)) {
                // never time out; wait as long as it takes for a coordinator to become available
                socket.setSoTimeout(0);

                while (!stopped) {
                    socket.receive(dataIn);

                    String msg = new String(buffer).substring(0, dataIn.getLength());

                    // if the UDP packet arrives on the expected port and the data appears to be formatted as JSON,
                    // assume it is a Semantic Synchrony message
                    if (msg.startsWith("{") && msg.endsWith("}")) {
                        logger.fine("UDP message received: " + msg);

                        try {
                            ServiceDescription d = new ServiceDescription(msg);

                            eventHandler.receivedServiceDescription(dataIn.getAddress(), d);
                        } catch (ServiceDescription.InvalidServiceDescriptionException e) {
                            logger.warning("invalid service description in datagram from "
                                    + dataIn.getAddress().getHostAddress() + ": " + msg);
                        }
                    }
                }
            }
        } catch (IOException e) {
            logger.severe("IOException while waiting for broadcast datagram from coordinator: " + e.getMessage());
        }
    }

    public interface EventHandler {
        void receivedServiceDescription(InetAddress address,
                                        ServiceDescription description);
    }
}
