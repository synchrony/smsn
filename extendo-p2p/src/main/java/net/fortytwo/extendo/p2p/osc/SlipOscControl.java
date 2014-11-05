package net.fortytwo.extendo.p2p.osc;

import com.illposed.osc.OSCMessage;
import net.fortytwo.extendo.util.SlipOutputStream;

import java.io.OutputStream;

/**
 * A controller for a remote device or component,
 * which communicates with it via OSC messages sent and received using the SLIP protocol
 *
 * @author Joshua Shinavier (http://fortytwo.net)
 */
public abstract class SlipOscControl {
    private final OSCDispatcher dispatcher;
    private SlipOutputStream slipStream;

    // TODO: temporary
    protected long timeOfLastEvent;

    /**
     * @param dispatcher an OSC dispatcher through which to receive messages
     */
    public SlipOscControl(final OSCDispatcher dispatcher) {
        this.dispatcher = dispatcher;
    }

    public void connect(final OutputStream outputStream) {
        this.slipStream = new SlipOutputStream(outputStream);

        onConnect();
    }

    public void disconnect() {
        this.slipStream = null;
    }

    // override this method
    protected void onConnect() {
    }

    public void send(final OSCMessage message) {
        dispatcher.send(message, slipStream);
    }

    public class DeviceInitializationException extends Exception {
        public DeviceInitializationException(final Throwable cause) {
            super(cause);
        }
    }
}
