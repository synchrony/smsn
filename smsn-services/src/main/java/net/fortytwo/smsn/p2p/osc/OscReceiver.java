package net.fortytwo.smsn.p2p.osc;

import com.illposed.osc.OSCMessage;
import com.illposed.osc.OSCPacket;
import com.illposed.osc.utility.OSCByteArrayToJavaConverter;

import java.util.HashMap;
import java.util.HashSet;
import java.util.Map;
import java.util.Set;
import java.util.logging.Level;
import java.util.logging.Logger;

public class OscReceiver {

    private static final Logger logger = Logger.getLogger(OscReceiver.class.getName());

    private final Map<String, OscMessageHandler> handlers;

    private final Set<OSCMessageListener> listeners;

    public OscReceiver() {
        handlers = new HashMap<>();
        listeners = new HashSet<>();
    }

    public void register(final String oscAddress,
                         final OscMessageHandler handler) {
        // note: no checking for duplicate addresses

        handlers.put(oscAddress, handler);
    }

    public void addListener(final OSCMessageListener listener) {
        listeners.add(listener);
    }

    public void removeListener(final OSCMessageListener listener) {
        listeners.remove(listener);
    }

    /**
     * Routes an OSC message to the appropriate handler
     *
     * @param message the received message to be handled
     * @return whether a matching handler was found
     */
    public boolean receive(final OSCMessage message) {
        OscMessageHandler handler = handlers.get(message.getAddress());

        boolean handled;
        if (null == handler) {
            handled = false;
        } else {
            handler.handle(message);
            handled = true;
        }

        // give the message to the listeners after any time-critical handlers have been served
        for (OSCMessageListener listener : listeners) {
            listener.handle(message);
        }

        return handled;
    }

    public boolean receive(final OSCPacket p) {
        if (p instanceof OSCMessage) {
            return receive((OSCMessage) p);
        } else {
            logger.log(Level.WARNING, "OSC packet is of non-message type " + p.getClass().getSimpleName() + ": " + p);
            return false;
        }
    }

    public boolean receive(final byte[] data, int length) {
        OSCByteArrayToJavaConverter c = new OSCByteArrayToJavaConverter();

        OSCPacket p = c.convert(data, length);

        return receive(p);
    }

    public interface OSCMessageListener {
        void handle(OSCMessage m);
    }
}
