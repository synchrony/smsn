package net.fortytwo.extendo.server;

import com.tinkerpop.rexster.extension.ExtensionResponse;
import org.json.JSONObject;

/**
 * @author Joshua Shinavier (http://fortytwo.net)
 */
public interface RequestHandler {
    ExtensionResponse handle(JSONObject message) throws RequestHandlerException;

    public class RequestHandlerException extends Exception {
        public RequestHandlerException(final String message) {
            super(message);
        }

        public RequestHandlerException(final Throwable cause) {
            super(cause);
        }
    }
}
