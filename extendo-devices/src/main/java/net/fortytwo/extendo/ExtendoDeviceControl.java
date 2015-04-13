package net.fortytwo.extendo;

import com.illposed.osc.OSCMessage;
import net.fortytwo.extendo.p2p.ExtendoAgent;
import net.fortytwo.extendo.p2p.osc.OscControl;
import net.fortytwo.extendo.p2p.osc.OscMessageHandler;
import net.fortytwo.extendo.p2p.osc.OscReceiver;

import java.util.List;
import java.util.logging.Level;

/**
 * A base class for controllers of Extendo devices, including support for common messages and handlers
 */
public abstract class ExtendoDeviceControl extends OscControl {

    // for now, queries will not expire, and will not need to be renewed
    protected static final int QUERY_TTL = 0;

    // outbound paths: for messages to the device
    private static final String
            OSC_MORSE = "/morse",
            OSC_OK = "/ok",
            OSC_PING = "/ping",
            OSC_READY = "/ready",
            OSC_VIBRO = "/vibro",
            OSC_WARNING = "/warning";

    // inbound paths: for messages from the device
    private static final String
            OSC_ERROR = "/error",  // note: also used as an outbound address
            OSC_INFO = "/info",  // note: also used as an outbound address
            OSC_PING_REPLY = "/ping/reply";

    private final String oscAddress;
    protected final ExtendoAgent agent;

    private long latestPing;

    protected ExtendoDeviceControl(final String oscAddress,
                                   final OscReceiver receiver,
                                   final ExtendoAgent agent) {
        super(receiver);

        this.oscAddress = oscAddress;
        this.agent = agent;

        registerCommonHandlers();
    }

    public ExtendoAgent getAgent() {
        return agent;
    }

    @Override
    protected void onConnect() {
        sendPingMessage();
    }

    protected void registerCommonHandlers() {
        receiver.register(absoluteAddress(OSC_ERROR), new OscMessageHandler() {
            public void handle(OSCMessage message) {
                List<Object> args = message.getArguments();
                if (wrongArgs(OSC_ERROR, 1, args.size())) {
                    return;
                }

                logger.log(Level.SEVERE, "error message from Extend-o-Hand: " + args.get(0));
            }
        });

        receiver.register(absoluteAddress(OSC_INFO), new OscMessageHandler() {
            public void handle(OSCMessage message) {
                List<Object> args = message.getArguments();
                if (wrongArgs(OSC_INFO, 1, args.size())) {
                    return;
                }

                logger.log(Level.INFO, "info message from Extend-o-Hand: " + args.get(0));
            }
        });

        receiver.register(absoluteAddress(OSC_PING), new OscMessageHandler() {
            public void handle(OSCMessage message) {
                // note: currently, no argument is provided, or needed;
                // the ping is used by the Extend-o-Hand to notify the user of a connection
                logger.info("ping received from Extend-o-Hand. Replying.");
                sendPingReplyMessage();
            }
        });

        receiver.register(absoluteAddress(OSC_PING_REPLY), new OscMessageHandler() {
            public void handle(OSCMessage message) {
                // note: argument is ignored for now; in future, it could be used to synchronize clocks

                // we assume this reply is a response to the latest ping
                // TODO: we don't have to... why not send and receive latestPing in the message
                long delay = System.currentTimeMillis() - latestPing;

                logger.log(Level.INFO, "ping reply received from Extend-o-Hand in " + delay + "ms");
            }
        });
    }

    public void sendErrorMessage() {
        OSCMessage m = new OSCMessage(absoluteAddress(OSC_ERROR));
        send(m);
    }

    public void sendInfoMessage() {
        OSCMessage m = new OSCMessage(absoluteAddress(OSC_INFO));
        send(m);
    }

    public void sendMorseMessage(final String text) {
        OSCMessage m = new OSCMessage(absoluteAddress(OSC_MORSE));
        m.addArgument(text);
        send(m);
    }

    public void sendOkMessage() {
        OSCMessage m = new OSCMessage(absoluteAddress(OSC_OK));
        send(m);
    }

    public void sendPingMessage() {
        OSCMessage message = new OSCMessage(absoluteAddress(OSC_PING));
        latestPing = System.currentTimeMillis();
        message.addArgument(latestPing);
        send(message);
    }

    public void sendPingReplyMessage() {
        OSCMessage message = new OSCMessage(absoluteAddress(OSC_PING_REPLY));
        // note: currently, no argument is consumed by the Typeatron
        send(message);
    }

    public void sendReadyMessage() {
        OSCMessage m = new OSCMessage(absoluteAddress(OSC_READY));
        send(m);
    }

    /**
     * @param time the duration of the signal in milliseconds (valid values range from 1 to 60000)
     */
    public void sendVibroMessage(final int time) {
        if (time < 0 || time > 60000) {
            throw new IllegalArgumentException("vibration interval too short or too long: " + time);
        }

        OSCMessage m = new OSCMessage(absoluteAddress(OSC_VIBRO));
        m.addArgument(time);
        send(m);
    }

    public void sendWarningMessage() {
        OSCMessage m = new OSCMessage(absoluteAddress(OSC_WARNING));
        send(m);
    }

    protected String absoluteAddress(final String path) {
        return oscAddress + path;
    }

    protected boolean wrongArgs(final String path,
                                final int expected,
                                final int actual) {
        if (actual != expected) {
            logger.log(Level.SEVERE, "received " + actual + " arguments in " + absoluteAddress(path) + " message, "
                    + "expected " + expected);
            return true;
        } else {
            return false;
        }
    }
}
