package net.fortytwo.smsn.p2p.osc;

import com.illposed.osc.OSCBundle;
import com.illposed.osc.OSCMessage;

import java.util.concurrent.BlockingQueue;
import java.util.concurrent.LinkedBlockingQueue;
import java.util.logging.Level;
import java.util.logging.Logger;

/**
 * A controller for a remote device or component, which communicates with it via Open Sound Control (OSC) messages
 *
 * @author Joshua Shinavier (http://fortytwo.net)
 */
public abstract class OscControl {

    protected static final Logger logger = Logger.getLogger(SlipOscSender.class.getName());

    protected final OscReceiver receiver;
    protected OscSender sender;

    private long throttlingPeriod = 100;
    private long timeOfLastPacket;

    // TODO: temporary
    protected long timeOfLastEvent;

    private BlockingQueue<OSCMessage> messages;

    /**
     * @param receiver a handler for incoming OSC messages
     */
    protected OscControl(final OscReceiver receiver) {
        this.receiver = receiver;
    }

    /**
     * Returns the handler for incoming OSC messages
     *
     * @return the handler for incoming OSC messages
     */
    public OscReceiver getReceiver() {
        return receiver;
    }

    /**
     * Sets the period for synchronous (by default) or asychronous throttling
     *
     * @param throttlingPeriod a minimum interval between outgoing messages
     */
    public void setThrottlingPeriod(final long throttlingPeriod) {
        if (0 > throttlingPeriod) {
            throw new IllegalArgumentException();
        }

        this.throttlingPeriod = throttlingPeriod;
    }

    /**
     * Start a separate thread to throttle outgoing messages, keeping them in a buffer of limited capacity and dropping
     * messages above that capacity.
     * Messages do not follow each other more closely than the given throttling period.
     *
     * @param bufferCapacity the maximum number of messages in a waiting queue of messages to be sent.
     *                       Additional messages are dropped until the queue shrinks.
     */
    public void throttleAsynchronously(final int bufferCapacity) {
        if (1 >= bufferCapacity) {
            throw new IllegalArgumentException();
        }

        if (0 == throttlingPeriod) {
            throw new IllegalStateException("throttling period must be positive");
        }

        final OscControl self = this;
        messages = new LinkedBlockingQueue<>(bufferCapacity);

        new Thread(() -> {
            try {
                while (self.throttlingPeriod > 0) {
                    OSCMessage message = messages.take();

                    throttle();

                    sendInternal(message);
                }
            } catch (Throwable t) {
                logger.log(Level.SEVERE, "message throttling thread died with error", t);
            }
        }).start();
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
        if (null == sender) {
            logger.warning("can't send message with address " + message.getAddress() + "; no sender has been defined");
        } else {
            if (null != messages) {
                messages.offer(message);
            } else {
                if (throttlingPeriod > 0) {
                    throttle();
                }

                sendInternal(message);
            }
        }
    }

    private void sendInternal(final OSCMessage message) {
        //System.out.println("" + System.currentTimeMillis() + " sending OSC message to " + message.getAddress() + " with " + message.getArguments().size() + " args");

        OSCBundle bundle = new OSCBundle();
        bundle.addPacket(message);
        sender.send(bundle);
    }

    private void throttle() {
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

    public class DeviceInitializationException extends Exception {
        public DeviceInitializationException(final Throwable cause) {
            super(cause);
        }

        public DeviceInitializationException(final String message, final Throwable cause) {
            super(message, cause);
        }
    }
}
