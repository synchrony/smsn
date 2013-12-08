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

    private final ServiceDescription serviceDescription;

    private boolean stopped;

    public ServiceBroadcaster(final ServiceDescription serviceDescription) {
        this.serviceDescription = serviceDescription;
    }

    public void start() {
        stopped = false;

        new Thread(new Runnable() {
            public void run() {
                LOGGER.info("listening for facilitator broadcast messages");

                try {
                    sendBroadcastMessages();
                } catch (Throwable t) {
                    LOGGER.severe("broadcast message listener thread failed: " + t.getMessage());
                    t.printStackTrace(System.err);
                }

                LOGGER.info("broadcast message listener stopped");
            }
        }).start();
    }

    public void stop() {
        stopped = true;
    }

    private void sendBroadcastMessages() {
        try {
            DatagramSocket socket = new DatagramSocket();
            socket.setBroadcast(true);

            try {
                long broadcastInterval = Extendo.getConfiguration().getLong(Extendo.P2P_BROADCAST_INTERVAL);

                JSONObject j = serviceDescription.toJSON();
                byte[] buffer = j.toString().getBytes();

                // TODO: temporary
                InetAddress broadcastAddress = InetAddress.getByName("10.0.2.255");

                int port = Extendo.getConfiguration().getInt(Extendo.P2P_BROADCAST_PORT);

                DatagramPacket packet;
                packet = new DatagramPacket(buffer, buffer.length, broadcastAddress, port);

                while (!stopped) {
                    LOGGER.fine("sending broadcast message: " + j);
                    socket.send(packet);

                    try {
                        Thread.sleep(broadcastInterval);
                    } catch (InterruptedException e) {
                        LOGGER.warning("error while waiting to send next broadcast: " + e.getMessage());
                        e.printStackTrace(System.err);
                        break;
                    }
                }
            } finally {
                socket.close();
            }
        } catch (IOException e) {
            LOGGER.severe("error while sending broadcast message(s): " + e.getMessage());
            e.printStackTrace(System.err);
        } catch (JSONException e) {
            LOGGER.severe("error creating JSON content of broadcast message: " + e.getMessage());
            e.printStackTrace(System.err);
        } catch (PropertyException e) {
            LOGGER.severe("error accessing config properties when sending broadcast message: " + e.getMessage());
            e.printStackTrace(System.err);
        }
    }
}
