package net.fortytwo.extendo.p2p.osc;

import com.illposed.osc.OSCBundle;
import com.illposed.osc.OSCMessage;

import java.util.logging.Logger;

/**
 * A controller for a remote device or component,
 * which communicates with it via OSC messages sent and received using the SLIP protocol
 *
 * @author Joshua Shinavier (http://fortytwo.net)
 */
public abstract class OscControl {

    protected static final Logger logger = Logger.getLogger(SlipOscSender.class.getName());

    private final OscReceiver receiver;
    private OscSender sender;

    // TODO: temporary
    protected long timeOfLastEvent;

    /**
     * @param receiver a handler for incoming OSC messages
     */
    protected OscControl(final OscReceiver receiver) {
        this.receiver = receiver;
    }

    /**
     * Returns the handler for incoming OSC messages
     * @return the handler for incoming OSC messages
     */
    public OscReceiver getReceiver() {
        return receiver;
    }

    public void connect(OscSender sender) {
        this.sender = sender;

        onConnect();
    }

    // override this method
    protected void onConnect() {
    }

    public void send(final OSCMessage message) {
        if (null == sender) {
            logger.warning("can't send message with address " + message.getAddress() + "; no sender has been defined");
        } else {
            OSCBundle bundle = new OSCBundle();
            bundle.addPacket(message);
            sender.send(bundle);
        }
    }

    public class DeviceInitializationException extends Exception {
        public DeviceInitializationException(final Throwable cause) {
            super(cause);
        }
    }

}
