package net.fortytwo.extendo.brainstem;

import com.illposed.osc.OSCMessage;

import java.util.HashMap;
import java.util.Map;

/**
 * @author Joshua Shinavier (http://fortytwo.net)
 */
public class OSCDispatcher {
    private final Map<String, OSCMessageHandler> handlers;

    public OSCDispatcher() {
        handlers = new HashMap<String, OSCMessageHandler>();
    }

    public void register(final String address,
                         final OSCMessageHandler handler) {
        // note: no checking for duplicate addresses

        handlers.put(address, handler);
    }

    /**
     * Route an OSC message to the appropriate handler
     * @param message the received message to be handled
     * @return whether a matching handler was found
     */
    public boolean dispatch(final OSCMessage message) {
        OSCMessageHandler handler = handlers.get(message.getAddress());

        if (null == handler) {
            return false;
        } else {
            handler.handle(message);
            return true;
        }
    }
}
