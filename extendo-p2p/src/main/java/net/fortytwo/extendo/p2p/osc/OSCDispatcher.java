package net.fortytwo.extendo.p2p.osc;

import com.illposed.osc.OSCBundle;
import com.illposed.osc.OSCMessage;
import com.illposed.osc.OSCPacket;
import com.illposed.osc.utility.OSCByteArrayToJavaConverter;
import net.fortytwo.extendo.util.SlipOutputStream;

import java.io.IOException;
import java.util.HashMap;
import java.util.HashSet;
import java.util.Map;
import java.util.Set;
import java.util.logging.Level;
import java.util.logging.Logger;

/**
 * @author Joshua Shinavier (http://fortytwo.net)
 */
public class OSCDispatcher {

    protected static final Logger logger = Logger.getLogger(OSCDispatcher.class.getName());

    private static final int SPP_PAYLOAD_CAPACITY = 128;

    private final Map<String, OSCMessageHandler> handlers;

    private final Set<OSCMessageListener> listeners;

    private boolean verbose;

    public OSCDispatcher() {
        handlers = new HashMap<String, OSCMessageHandler>();
        listeners = new HashSet<OSCMessageListener>();
    }

    public void setVerbose(final boolean verbose) {
        this.verbose = verbose;
    }

    public void register(final String oscAddress,
                         final OSCMessageHandler handler) {
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
    public boolean dispatch(final OSCMessage message) {
        OSCMessageHandler handler = handlers.get(message.getAddress());

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

    public void receive(final OSCPacket p) {
        if (p instanceof OSCMessage) {
            if (verbose) {
                logger.log(Level.INFO, "received OSC message: " + toString((OSCMessage) p));
            }

            if (!dispatch((OSCMessage) p)) {
                logger.log(Level.WARNING, "no OSC handler at address " + ((OSCMessage) p).getAddress());
            }
        } else {
            logger.log(Level.WARNING, "OSC packet is of non-message type " + p.getClass().getSimpleName() + ": " + p);
        }
    }

    public void receive(final byte[] data, int length) {
        OSCByteArrayToJavaConverter c = new OSCByteArrayToJavaConverter();

        OSCPacket p = c.convert(data, length);

        receive(p);
    }

    public void send(final OSCMessage message,
                     final SlipOutputStream slipStream) {
        if (verbose) {
            logger.log(Level.INFO, "sending OSC message: " + toString(message));
        }

        if (null == slipStream) {
            logger.log(Level.WARNING, "can't send OSC message; no output stream");
        } else {
            try {
                //messageWriter.sendMessage(message.getByteArray());

                // Arduino-based Extendo devices receive OSC bundles and send OSC messages
                OSCBundle bundle = new OSCBundle();
                bundle.addPacket(message);

                byte[] bytes = bundle.getByteArray();

                // TODO: this warning is only relevant in a Bluetooth context
                if (bytes.length >= SPP_PAYLOAD_CAPACITY) {
                    // SLIP will expand a message by at least one byte (for END),
                    // sometimes many bytes (depending on the number of escape sequences required)
                    logger.log(Level.WARNING, "message length (" + bytes.length
                            + " bytes) should be kept well under Bluetooth SPP payload capacity (128 bytes)");
                }

                slipStream.send(bundle.getByteArray());
            } catch (IOException e) {
                logger.log(Level.SEVERE, "I/O error while sending OSC message", e);
            }
        }
    }

    private String toString(final OSCMessage message) {
        StringBuilder sb = new StringBuilder(message.getAddress());

        for (Object arg : message.getArguments()) {
            sb.append(" ");
            sb.append(arg);
        }

        return sb.toString();
    }

    public interface OSCMessageListener {
        void handle(OSCMessage m);
    }
}
