package net.fortytwo.smsn.p2p;

import org.json.JSONObject;

/**
* @author Joshua Shinavier (http://fortytwo.net)
*/
public interface MessageHandler {
    void handle(JSONObject message) throws MessageHandlerException;

    public class MessageHandlerException extends Exception {
        public MessageHandlerException(final String message) {
            super(message);
        }

        public MessageHandlerException(final Throwable cause) {
            super(cause);
        }
    }
}
