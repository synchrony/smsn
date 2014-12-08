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

    private long throttlingPeriod;
    private long timeOfLastPacket;

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

    public void setThrottlingPeriod(final long throttlingPeriod) {
        this.throttlingPeriod = throttlingPeriod;
    }

    /**
     * Attaches an OSC sender, also calling the onConnect method which may be used to initialize this control
     *
     * @param sender the new OSC sender
     */
    public void connect(final OscSender sender) {
        this.sender = sender;

        onConnect();
    }

    /**
     * Closes and detaches the OSC sender, if one has previously been attached with <code>connect()</code>
     */
    public synchronized void disconnect() {
        if (null != sender) {
            sender.close();
            sender = null;
        }
    }

    // override this method
    protected void onConnect() {
    }

    /**
     * Passes an OSC message to the attached sender.
     * Note: this method is synchronized for the sake of senders of uncertain thread safety
     *
     * @param message the OSC message to send
     */
    public synchronized void send(final OSCMessage message) {
        // TODO: temporary
        System.out.println("sending OSC message to " + message.getAddress() + " with " + message.getArguments().size() + " args");

        if (null == sender) {
            logger.warning("can't send message with address " + message.getAddress() + "; no sender has been defined");
        } else {
            throttle();

            OSCBundle bundle = new OSCBundle();
            bundle.addPacket(message);
            sender.send(bundle);
        }
    }

    private void throttle() {
        if (throttlingPeriod > 0) {
            long now = System.currentTimeMillis();
            if (now - timeOfLastPacket < throttlingPeriod) {
                try {
                    Thread.sleep(throttlingPeriod - (now - timeOfLastPacket));
                } catch (InterruptedException e) {
                    throw new IllegalStateException(e);
                }
            }
            timeOfLastPacket = now;
        }
    }

    public class DeviceInitializationException extends Exception {
        public DeviceInitializationException(final Throwable cause) {
            super(cause);
        }
    }
}
