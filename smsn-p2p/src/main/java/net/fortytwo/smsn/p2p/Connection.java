package net.fortytwo.smsn.p2p;

import net.fortytwo.smsn.SemanticSynchrony;
import org.json.JSONException;
import org.json.JSONObject;

import java.io.BufferedReader;
import java.io.IOException;
import java.io.InputStreamReader;
import java.net.Socket;
import java.util.HashMap;
import java.util.LinkedList;
import java.util.List;
import java.util.Map;
import java.util.logging.Level;
import java.util.logging.Logger;

public class Connection {
    private static final Logger logger = Logger.getLogger(Connection.class.getName());

    private Socket socket;

    private final Map<String, MessageHandler> handlers;

    private final List<BufferedMessage> buffer;

    private boolean stopped = false;

    public Connection() {
        handlers = new HashMap<>();
        buffer = new LinkedList<>();
    }

    /**
     * @return this connection's socket, or null if the connection is not active
     */
    public Socket getSocket() {
        return socket;
    }

    public boolean isActive() {
        return null != socket;
    }

    private void setSocket(final Socket socket) {
        this.socket = socket;
    }

    public synchronized void start(final Socket socket) {
        if (isActive()) {
            throw new IllegalStateException("connection is already active");
        }

        setSocket(socket);

        for (BufferedMessage bm : buffer) {
            try {
                sendInternal(bm.tag, bm.body);
            } catch (JSONException | IOException e) {
                logger.log(Level.WARNING, "error sending buffered message", e);
            }
        }
        buffer.clear();

        stopped = false;

        new Thread(() -> {
            logger.info("starting message handler thread");

            try {
                // TODO: recover from IO errors (e.g. due to temporary network issues)
                handleIncomingMessages();
            } catch (Throwable e) {
                logger.severe("message handler thread failed with error: " + e.getMessage());
                e.printStackTrace(System.err);
            } finally {
                logger.info("message handler thread stopped");
            }

            // make this connection inactive
            setSocket(null);
        }).start();
    }

    public synchronized void stop() {
        if (!isActive()) {
            throw new IllegalStateException("connection is not active");
        }

        stopped = true;
    }

    public void registerHandler(final String tag,
                                final MessageHandler handler) {
        if (null != handlers.get(tag)) {
            throw new IllegalStateException("a '" + tag + "' handler is already registered with this connection");
        }

        handlers.put(tag, handler);
    }

    public synchronized void sendNow(final String tag,
                                     final JSONObject body) throws JSONException, IOException {
        if (!isActive()) {
            logger.fine("can't send; connection is closed");
            return;
        }

        sendInternal(tag, body);
    }

    public synchronized void sendBuffered(final String tag,
                                          final JSONObject body) throws IOException, JSONException {
        if (isActive()) {
            sendInternal(tag, body);
        } else {
            BufferedMessage bm = new BufferedMessage();
            bm.tag = tag;
            bm.body = body;
            buffer.add(bm);
        }
    }

    private void sendInternal(final String tag,
                              final JSONObject body) throws JSONException, IOException {
        JSONObject message = new JSONObject();
        message.put(SmSnAgent.PROP_TAG, tag);
        message.put(SmSnAgent.PROP_BODY, body);

        String s = message.toString();

        if (SemanticSynchrony.VERBOSE) {
            logger.info("sending message to " + socket.getRemoteSocketAddress() + ": " + s);
        }

        socket.getOutputStream().write(s.getBytes());
        socket.getOutputStream().write(10);
        socket.getOutputStream().flush();
    }

    private void close() throws IOException {
        socket.close();

        socket = null;
    }

    private void handleIncomingMessages() throws IOException {
        BufferedReader br = new BufferedReader(new InputStreamReader(socket.getInputStream()));

        while (!stopped) {
            String line = br.readLine();

            if (null == line) {
                logger.info("connection to " + socket.getRemoteSocketAddress() + " closed remotely");
                break;
            }

            if (SemanticSynchrony.VERBOSE) {
                logger.info("received message from " + socket.getRemoteSocketAddress() + ": " + line);
            }

            JSONObject message;
            try {
                message = new JSONObject(line);
            } catch (JSONException e) {
                logger.warning("could not parse message as JSON: " + e.getMessage());
                continue;
            }

            String tag;
            try {
                tag = message.getString(SmSnAgent.PROP_TAG);
            } catch (JSONException e) {
                logger.warning("missing '" + SmSnAgent.PROP_TAG + "' in JSON message. Discarding");
                continue;
            }

            JSONObject body;
            try {
                body = message.getJSONObject(SmSnAgent.PROP_BODY);
            } catch (JSONException e) {
                logger.warning("missing '" + SmSnAgent.PROP_BODY + "' in JSON message. Discarding");
                continue;
            }

            MessageHandler handler = handlers.get(tag);

            if (null == handler) {
                logger.warning("no handler for message with tag '" + tag + "'");
            } else {
                try {
                    handler.handle(body);
                } catch (MessageHandler.MessageHandlerException e) {
                    logger.severe("JSON message handler failed with error: " + e.getMessage());
                } catch (Throwable t) {
                    // don't allow otherwise uncaught handler errors to kill this loop/thread
                    logger.severe("JSON message handler failed with unexpected error: " + t.getMessage());
                    t.printStackTrace(System.err);
                }
            }
        }

        close();
    }

    private class BufferedMessage {
        public String tag;
        public JSONObject body;
    }

}
