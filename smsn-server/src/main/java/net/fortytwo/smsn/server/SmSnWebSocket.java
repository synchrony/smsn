package net.fortytwo.smsn.server;

import org.eclipse.jetty.websocket.api.Session;
import org.eclipse.jetty.websocket.api.WebSocketListener;
import org.json.JSONObject;

import java.io.IOException;
import java.util.logging.Level;
import java.util.logging.Logger;

/**
 * WebSocket handler for SmSn client connections.
 * Handles both Gremlin Server format (for Web UI compatibility) and raw JSON.
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
            // Handle Gremlin Server message format from Web UI
            // Web UI sends: {"gremlin": "{...action JSON...}"}
            String actionJson = unwrapGremlinMessage(message);
            String response = server.handleRequest(actionJson);

            // Wrap response in Gremlin Server format for Web UI compatibility
            String wrappedResponse = wrapGremlinResponse(response);
            session.getRemote().sendString(wrappedResponse);
        } catch (IOException e) {
            logger.log(Level.SEVERE, "Error sending response", e);
        }
    }

    /**
     * Unwrap a Gremlin Server format message to get the action JSON.
     * Gremlin Server format: {"gremlin": "{...escaped JSON...}"}
     */
    private String unwrapGremlinMessage(String message) {
        try {
            JSONObject wrapper = new JSONObject(message);
            if (wrapper.has("gremlin")) {
                return wrapper.getString("gremlin");
            }
        } catch (Exception e) {
            // Not a wrapped message, return as-is
        }
        return message;
    }

    /**
     * Wrap response in Gremlin Server format for Web UI compatibility.
     * Gremlin Server returns: {"result": {"data": {...}}, "status": {"code": 200}}
     */
    private String wrapGremlinResponse(String response) {
        try {
            JSONObject responseObj = new JSONObject(response);
            JSONObject wrapper = new JSONObject();

            JSONObject result = new JSONObject();
            result.put("data", responseObj);
            wrapper.put("result", result);

            JSONObject status = new JSONObject();
            status.put("code", 200);
            status.put("message", "");
            wrapper.put("status", status);

            return wrapper.toString();
        } catch (Exception e) {
            // If wrapping fails, return original
            logger.warning("Failed to wrap response: " + e.getMessage());
            return response;
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
