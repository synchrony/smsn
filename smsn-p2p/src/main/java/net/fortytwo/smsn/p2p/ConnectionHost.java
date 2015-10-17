package net.fortytwo.smsn.p2p;

import net.fortytwo.smsn.util.TypedProperties;

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
    protected static final Logger logger = Logger.getLogger(ConnectionHost.class.getName());

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
                logger.info("starting connection listener thread on port " + port);

                try {
                    listenForNewConnections();
                } catch (Throwable e) {
                    logger.severe("connection listener thread on port " + port
                            + " failed with error: " + e.getMessage());
                    e.printStackTrace(System.err);
                } finally {
                    logger.info("connection listener thread on port " + port + " has stopped");
                }
            }
        }).start();
    }

    public synchronized void stop() {
        try {
            closeAllConnections();
        } catch (IOException e) {
            logger.warning("error while closing connection(s): " + e.getMessage());
            e.printStackTrace(System.err);
        }

        stopped = true;
    }

    private void listenForNewConnections() throws TypedProperties.PropertyException, IOException {
        logger.info("listening for new connections on port " + port);
        // TODO: recover from network failure (IO errors)
        ServerSocket serverSocket = new ServerSocket(port);
        while (!stopped) {
            // wait here for the next connection
            Socket socket = serverSocket.accept();

            Connection c = new Connection();
            c.start(socket);
            logger.info("new pub/sub connection opened to "
                    + socket.getRemoteSocketAddress()
                    + " (" + socket.getInetAddress() + ")");
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
