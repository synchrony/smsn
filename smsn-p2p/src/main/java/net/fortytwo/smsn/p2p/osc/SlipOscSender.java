package net.fortytwo.smsn.p2p.osc;

import com.illposed.osc.OSCBundle;
import net.fortytwo.smsn.util.slip.SlipOutputStream;

import java.io.IOException;
import java.io.OutputStream;
import java.util.logging.Level;
import java.util.logging.Logger;

public class SlipOscSender implements OscSender {

    private static final Logger logger = Logger.getLogger(SlipOscSender.class.getName());

    private static final int SPP_PAYLOAD_CAPACITY = 128;

    private final SlipOutputStream slipStream;

    public SlipOscSender(final SlipOutputStream slipStream) {
        this.slipStream = slipStream;
    }

    public SlipOscSender(final OutputStream outputStream,
                         final boolean threaded) {
        this(new SlipOutputStream(outputStream, threaded));
    }

    @Override
    public synchronized void send(final OSCBundle bundle) {
        if (null == slipStream) {
            logger.log(Level.WARNING, "can't send OSC message; no output stream");
        } else {
            try {
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

    public void close() {
        try {
            slipStream.close();
        } catch (IOException e) {
            logger.log(Level.WARNING, "error on closing of SLIP stream", e);
        }
    }
}
