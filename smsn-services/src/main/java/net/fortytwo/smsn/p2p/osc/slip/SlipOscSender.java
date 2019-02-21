package net.fortytwo.smsn.p2p.osc.slip;

import com.illposed.osc.OSCBundle;
import com.illposed.osc.OSCSerializeException;
import com.illposed.osc.OSCSerializer;
import com.illposed.osc.OSCSerializerFactory;
import net.fortytwo.smsn.p2p.osc.OscSender;

import java.io.IOException;
import java.io.OutputStream;
import java.nio.ByteBuffer;
import java.util.Arrays;
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
                final ByteBuffer bundleContents = ByteBuffer.allocate(SPP_PAYLOAD_CAPACITY + 1);
                final OSCSerializer oscSerializer = OSCSerializerFactory.createDefaultFactory().create(bundleContents);
                oscSerializer.write(bundle);

                // TODO: this warning is only relevant in a Bluetooth context
                if (bundleContents.position() >= SPP_PAYLOAD_CAPACITY) {
                    // SLIP will expand a message by at least one byte (for END),
                    // sometimes many bytes (depending on the number of escape sequences required)
                    logger.log(Level.WARNING, "message length (" + bundleContents.position()
                            + " bytes) should be kept well under Bluetooth SPP payload capacity (128 bytes)");
                }

                bundleContents.flip();
                slipStream.send(Arrays.copyOf(bundleContents.array(), bundleContents.limit()));
            } catch (IOException | OSCSerializeException e) {
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
