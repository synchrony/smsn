package net.fortytwo.smsn.hand;

import com.illposed.osc.OSCMessage;
import net.fortytwo.smsn.SmSnDeviceControl;
import net.fortytwo.smsn.p2p.SmSnAgent;
import net.fortytwo.smsn.p2p.osc.OscReceiver;

import java.util.logging.Logger;

/**
 * A controller for the Extend-o-Hand gestural glove
 */
public class ExtendoHandControl extends SmSnDeviceControl {
    protected static final Logger logger = Logger.getLogger(ExtendoHandControl.class.getName());

    private static final int
            MAX_FREQUENCY = 20000, // 20kHz
            MAX_DURATION = 60000; // 60s

    // outbound paths: for messages to the device
    private static final String
            OSC_ALERT = "/alert",
            OSC_MULTI = "/multi";

    public ExtendoHandControl(final OscReceiver oscReceiver,
                              final SmSnAgent agent) {
        super("/exo/hand", oscReceiver, agent);
    }

    public void sendAlertMessage() {
        OSCMessage message = new OSCMessage(absoluteAddress(OSC_ALERT));
        send(message);
    }

    public void sendMulticueMessage(final int toneFrequency,
                                    final int toneDurationMs,
                                    final int color,
                                    final int vibrationDurationMs) {
        if (toneFrequency < 0 || toneFrequency > 20000) {
            throw new IllegalArgumentException("tone frequency is out of range: " + vibrationDurationMs);
        }

        if (toneDurationMs < 0 || toneDurationMs > 60000) {
            throw new IllegalArgumentException("tone duration is out of range: " + toneDurationMs);
        }

        if (vibrationDurationMs < 0 || vibrationDurationMs > 60000) {
            throw new IllegalArgumentException("vibration duration is out of range: " + vibrationDurationMs);
        }

        OSCMessage m = new OSCMessage(absoluteAddress(OSC_MULTI));
        m.addArgument(toneFrequency);
        m.addArgument(toneDurationMs);
        m.addArgument(color);
        m.addArgument(vibrationDurationMs);
        send(m);
    }
}
