package net.fortytwo.smsn.p2p;

import net.fortytwo.smsn.SemanticSynchrony;
import net.fortytwo.smsn.util.TypedProperties;
import org.json.JSONException;
import org.json.JSONObject;

import java.io.IOException;
import java.net.DatagramPacket;
import java.net.DatagramSocket;
import java.net.InetAddress;
import java.util.logging.Logger;

/**
 * @author Joshua Shinavier (http://fortytwo.net)
 */
public class ServiceBroadcaster {
    protected static final Logger logger = Logger.getLogger(ServiceBroadcaster.class.getName());

    // TODO: make this configurable
    private static final long MAX_BACKOFF = 60000;

    private final ServiceDescription serviceDescription;

    private boolean stopped;

    public ServiceBroadcaster(final ServiceDescription serviceDescription) {
        this.serviceDescription = serviceDescription;
    }

    public void start() {
        stopped = false;

        new Thread(new Runnable() {
            public void run() {
                logger.info("starting service broadcaster thread");

                try {
                    sendBroadcastMessages();
                } catch (Throwable t) {
                    logger.severe("service broadcaster thread failed with error: " + t.getMessage());
                    t.printStackTrace(System.err);
                } finally {
                    logger.info("service broadcaster thread stopped");
                }
            }
        }).start();
    }

    public void stop() {
        stopped = true;
    }

    private void sendBroadcastMessages() {
        String address;
        int port;
        long broadcastInterval;

        try {
            address = SemanticSynchrony.getConfiguration().getString(SemanticSynchrony.P2P_BROADCAST_ADDRESS);
            port = SemanticSynchrony.getConfiguration().getInt(SemanticSynchrony.P2P_BROADCAST_PORT);
            broadcastInterval = SemanticSynchrony.getConfiguration().getLong(SemanticSynchrony.P2P_BROADCAST_INTERVAL);
        } catch (TypedProperties.PropertyException e) {
            logger.severe("error accessing config properties when sending broadcast message: " + e.getMessage());
            e.printStackTrace(System.err);
            return;
        }

        JSONObject j;
        try {
            j = serviceDescription.toJSON();
        } catch (JSONException e) {
            logger.severe("error creating JSON content of broadcast message: " + e.getMessage());
            e.printStackTrace(System.err);
            return;
        }
        byte[] buffer = j.toString().getBytes();

        long backoff = broadcastInterval;

        // outer loop recovers from IO errors
        while (!stopped) {
            try {
                DatagramPacket packet;
                packet = new DatagramPacket(buffer, buffer.length, InetAddress.getByName(address), port);

                DatagramSocket socket = new DatagramSocket();
                socket.setBroadcast(true);

                try {
                    // inner loop repeatedly sends a broadcast message in absence of errors
                    while (!stopped) {
                        logger.fine("sending broadcast message: " + j);
                        socket.send(packet);

                        backoff = broadcastInterval;

                        try {
                            Thread.sleep(broadcastInterval);
                        } catch (InterruptedException e) {
                            logger.warning("error while waiting to send next broadcast: " + e.getMessage());
                            e.printStackTrace(System.err);
                            return;
                        }
                    }
                } finally {
                    socket.close();
                }
            } catch (IOException e) {
                logger.warning("error while sending broadcast message(s): " + e.getMessage());
                //e.printStackTrace(System.err);
                backoff *= 2;
                if (backoff > MAX_BACKOFF) {
                    backoff = MAX_BACKOFF;
                }

                logger.info("waiting " + backoff + "ms before next broadcast");
                try {
                    Thread.sleep(backoff);
                } catch (InterruptedException e2) {
                    logger.warning("error while waiting to reopen broadcast socket: " + e.getMessage());
                    e2.printStackTrace(System.err);
                    return;
                }
            }
        }
    }
}
