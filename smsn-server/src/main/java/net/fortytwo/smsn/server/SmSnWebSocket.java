package net.fortytwo.smsn.server;

import org.eclipse.jetty.websocket.api.Session;
import org.eclipse.jetty.websocket.api.WebSocketListener;

import java.io.IOException;
import java.util.logging.Level;
import java.util.logging.Logger;

/**
 * WebSocket handler for SmSn client connections.
 */
public class SmSnWebSocket implements WebSocketListener {
    private static final Logger logger = Logger.getLogger(SmSnWebSocket.class.getName());

    private final SmSnServer server;
    private Session session;

    public SmSnWebSocket(SmSnServer server) {
        this.server = server;
    }

    @Override
    public void onWebSocketConnect(Session session) {
        this.session = session;
        logger.info("WebSocket connected: " + session.getRemoteAddress());
    }

    @Override
    public void onWebSocketText(String message) {
        logger.fine("Received message: " + message);

        try {
            String response = server.handleRequest(message);
            session.getRemote().sendString(response);
        } catch (IOException e) {
            logger.log(Level.SEVERE, "Error sending response", e);
        }
    }

    @Override
    public void onWebSocketBinary(byte[] payload, int offset, int len) {
        logger.warning("Binary messages not supported");
    }

    @Override
    public void onWebSocketClose(int statusCode, String reason) {
        logger.info("WebSocket closed: " + statusCode + " - " + reason);
        this.session = null;
    }

    @Override
    public void onWebSocketError(Throwable cause) {
        logger.log(Level.SEVERE, "WebSocket error", cause);
    }
}
