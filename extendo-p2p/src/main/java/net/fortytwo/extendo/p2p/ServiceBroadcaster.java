package net.fortytwo.extendo.p2p;

import net.fortytwo.extendo.Extendo;
import net.fortytwo.extendo.util.properties.PropertyException;
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
    protected static final Logger LOGGER = Logger.getLogger(ServiceBroadcaster.class.getName());

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
                LOGGER.info("starting service broadcaster thread");

                try {
                    sendBroadcastMessages();
                } catch (Throwable t) {
                    LOGGER.severe("service broadcaster thread failed: " + t.getMessage());
                    t.printStackTrace(System.err);
                }

                LOGGER.info("service broadcaster thread stopped");
            }
        }).start();
    }

    public void stop() {
        stopped = true;
    }

    private void sendBroadcastMessages() {
        long broadcastInterval;
        int port;

        try {
            broadcastInterval = Extendo.getConfiguration().getLong(Extendo.P2P_BROADCAST_INTERVAL);
            port = Extendo.getConfiguration().getInt(Extendo.P2P_BROADCAST_PORT);
        } catch (PropertyException e) {
            LOGGER.severe("error accessing config properties when sending broadcast message: " + e.getMessage());
            e.printStackTrace(System.err);
            return;
        }

        JSONObject j;
        try {
            j = serviceDescription.toJSON();
        } catch (JSONException e) {
            LOGGER.severe("error creating JSON content of broadcast message: " + e.getMessage());
            e.printStackTrace(System.err);
            return;
        }
        byte[] buffer = j.toString().getBytes();

        long backoff = broadcastInterval;

        // outer loop recovers from IO errors
        while (!stopped) {
            try {
                // TODO: temporary
                InetAddress broadcastAddress = InetAddress.getByName("10.0.2.255");

                DatagramPacket packet;
                packet = new DatagramPacket(buffer, buffer.length, broadcastAddress, port);

                DatagramSocket socket = new DatagramSocket();
                socket.setBroadcast(true);

                try {
                    // inner loop repeatedly sends a broadcast message in absence of errors
                    while (!stopped) {
                        LOGGER.fine("sending broadcast message: " + j);
                        socket.send(packet);

                        backoff = broadcastInterval;

                        try {
                            Thread.sleep(broadcastInterval);
                        } catch (InterruptedException e) {
                            LOGGER.warning("error while waiting to send next broadcast: " + e.getMessage());
                            e.printStackTrace(System.err);
                            return;
                        }
                    }
                } finally {
                    socket.close();
                }
            } catch (IOException e) {
                LOGGER.warning("error while sending broadcast message(s): " + e.getMessage());
                //e.printStackTrace(System.err);
                backoff *= 2;
                if (backoff > MAX_BACKOFF) {
                    backoff = MAX_BACKOFF;
                }

                LOGGER.info("waiting " + backoff + "ms before next broadcast");
                try {
                    Thread.sleep(backoff);
                } catch (InterruptedException e2) {
                    LOGGER.warning("error while waiting to reopen broadcast socket: " + e.getMessage());
                    e2.printStackTrace(System.err);
                    return;
                }
            }
        }
    }
}
