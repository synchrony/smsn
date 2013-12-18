package net.fortytwo.extendo.p2p;

import net.fortytwo.extendo.util.properties.PropertyException;

import java.io.IOException;
import java.net.ServerSocket;
import java.net.Socket;
import java.util.HashSet;
import java.util.Set;
import java.util.logging.Logger;

/**
 * @author Joshua Shinavier (http://fortytwo.net)
 */
public class ConnectionHost {
    protected static final Logger LOGGER = Logger.getLogger(ConnectionHost.class.getName());

    private final Set<Notifier> notifiers;

    // TODO: manage this set of connections, periodically checking which have been closed and removing them
    private final Set<Connection> connections = new HashSet<Connection>();

    private boolean stopped = true;

    private final int port;

    public ConnectionHost(final int port) {
        this.port = port;

        notifiers = new HashSet<Notifier>();
    }

    public void addNotifier(final Notifier notifier) {
        notifiers.add(notifier);
    }

    public synchronized void start() {
        if (!stopped) {
            return;
        }

        stopped = false;

        new Thread(new Runnable() {
            public void run() {
                LOGGER.info("starting connection listener thread");

                try {
                    listenForNewConnections();
                } catch (Throwable e) {
                    LOGGER.severe("connection listener thread failed with error: " + e.getMessage());
                    e.printStackTrace(System.err);
                } finally {
                    LOGGER.info("connection listener thread stopped");
                }
            }
        }).start();
    }

    public synchronized void stop() {
        try {
            closeAllConnections();
        } catch (IOException e) {
            LOGGER.warning("error while closing connection(s): " + e.getMessage());
            e.printStackTrace(System.err);
        }

        stopped = true;
    }

    private void listenForNewConnections() throws PropertyException, IOException {
        LOGGER.info("listening for new connections on port " + port);
        // TODO: recover from network failure (IO errors)
        ServerSocket serverSocket = new ServerSocket(port);
        //ServerSocket serverSocket = new ServerSocket(port, 0, InetAddress.getByName("192.168.1.136"));
        while (!stopped) {
            // wait here for the next connection
            Socket socket = serverSocket.accept();

            Connection c = new Connection();
            c.start(socket);
            LOGGER.info("new pub/sub connection opened to " + socket.getRemoteSocketAddress() + " (" + socket.getInetAddress() + ")");
            for (Notifier notifier : notifiers) {
                notifier.connectionCreated(c);
            }
        }
    }

    private void closeAllConnections() throws IOException {
        for (Connection c : connections) {
            if (!c.getSocket().isClosed()) {
                c.getSocket().close();
            }
        }

        connections.clear();
    }

    public interface Notifier {
        void connectionCreated(Connection c);
    }
}
