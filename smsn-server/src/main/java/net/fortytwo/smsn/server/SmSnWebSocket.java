package net.fortytwo.smsn.server;

import net.fortytwo.smsn.SemanticSynchrony;
import net.fortytwo.smsn.config.Authentication;
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
    private boolean authenticated;

    public SmSnWebSocket(SmSnServer server) {
        this.server = server;
        this.authenticated = false;
    }

    @Override
    public void onWebSocketConnect(Session session) {
        this.session = session;

        // Check for authentication token in query parameter
        Authentication auth = SemanticSynchrony.getConfiguration().getAuthentication();
        if (auth.isEnabled()) {
            String queryString = session.getUpgradeRequest().getQueryString();
            String providedToken = extractToken(queryString);
            String expectedToken = auth.getToken();

            if (expectedToken != null && expectedToken.equals(providedToken)) {
                this.authenticated = true;
                logger.info("WebSocket connected (authenticated): " + session.getRemoteAddress());
            } else {
                this.authenticated = false;
                logger.info("WebSocket connected (anonymous): " + session.getRemoteAddress());
            }
        } else {
            // Auth disabled - treat all connections as authenticated
            this.authenticated = true;
            logger.info("WebSocket connected: " + session.getRemoteAddress());
        }
    }

    /**
     * Extract token from query string (e.g., "token=SECRET" or "token=SECRET&other=value")
     */
    private String extractToken(String queryString) {
        if (queryString == null || queryString.isEmpty()) {
            return null;
        }
        for (String param : queryString.split("&")) {
            if (param.startsWith("token=")) {
                return param.substring(6);
            }
        }
        return null;
    }

    @Override
    public void onWebSocketText(String message) {
        logger.fine("Received message: " + message);

        try {
            // Handle Gremlin Server message format from Web UI
            // Web UI sends: {"gremlin": "{...action JSON...}"}
            String actionJson = unwrapGremlinMessage(message);
            String response = server.handleRequest(actionJson, authenticated);

            // Wrap response in Gremlin Server format for Web UI compatibility
            String wrappedResponse = wrapGremlinResponse(response);
            session.getRemote().sendString(wrappedResponse);
        } catch (IOException e) {
            if (isConnectionClosedError(e)) {
                logger.warning("Failed to send response (client disconnected): " + e.getClass().getSimpleName());
            } else {
                logger.log(Level.SEVERE, "Error sending response", e);
            }
        }
    }

    /**
     * Unwrap a Gremlin Server format message to get the action JSON.
     * Full Gremlin Server format: {"op":"eval","processor":"","args":{"language":"smsn","gremlin":"{...}"}}
     * Simple format: {"gremlin": "{...escaped JSON...}"}
     */
    private String unwrapGremlinMessage(String message) {
        try {
            JSONObject wrapper = new JSONObject(message);

            // Check for full Gremlin Server format with op/args
            if (wrapper.has("op") && wrapper.has("args")) {
                JSONObject args = wrapper.getJSONObject("args");
                if (args.has("gremlin")) {
                    return args.getString("gremlin");
                }
            }

            // Check for simple gremlin wrapper
            if (wrapper.has("gremlin")) {
                return wrapper.getString("gremlin");
            }
        } catch (Exception e) {
            // Not a wrapped message, return as-is
        }
        return message;
    }

    /**
     * Wrap response in Gremlin Server format for client compatibility.
     * Gremlin Server returns: {"result": {"data": ["...JSON string..."]}, "status": {"code": 200}}
     * The data field is an array containing the JSON response as a string.
     */
    private String wrapGremlinResponse(String response) {
        try {
            JSONObject wrapper = new JSONObject();

            JSONObject result = new JSONObject();
            // Gremlin Server returns data as an array with the JSON string as the first element
            org.json.JSONArray dataArray = new org.json.JSONArray();
            dataArray.put(response);
            result.put("data", dataArray);
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
        // Check for benign connection-closed errors (client disconnected during response)
        if (isConnectionClosedError(cause)) {
            logger.warning("WebSocket connection closed unexpectedly: " + cause.getClass().getSimpleName());
        } else {
            logger.log(Level.SEVERE, "WebSocket error", cause);
        }
    }

    /**
     * Check if the error is a benign connection-closed condition.
     * These occur when the client disconnects or the connection times out.
     */
    private boolean isConnectionClosedError(Throwable cause) {
        if (cause == null) {
            return false;
        }

        String className = cause.getClass().getSimpleName();
        String message = cause.getMessage();

        // EofException from Jetty
        if ("EofException".equals(className)) {
            return true;
        }

        // WebSocketTimeoutException - idle timeout, client will reconnect
        if (className.contains("TimeoutException")) {
            return true;
        }

        // Broken pipe or connection reset
        if (cause instanceof IOException) {
            if (message != null && (message.contains("Broken pipe") || message.contains("Connection reset"))) {
                return true;
            }
        }

        // Check for idle timeout in message
        if (message != null && message.contains("Idle Timeout")) {
            return true;
        }

        // Check the cause chain
        return isConnectionClosedError(cause.getCause());
    }
}
