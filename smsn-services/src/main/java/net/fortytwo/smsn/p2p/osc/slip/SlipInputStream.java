package net.fortytwo.smsn.p2p.osc.slip;

import java.io.IOException;
import java.io.InputStream;

/**
 * A utility for receiving byte array packets via the Serial Line Internet Protocol (SLIP)
 */
public class SlipInputStream {

    // a somewhat arbitrary UDP-inspired default
    private static final int DEFAULT_BUFFER_LENGTH = 1500;

    private static final int
            SLIP_END = 0xc0,
            SLIP_ESC = 0xdb,
            SLIP_ESC_END = 0xdc,
            SLIP_ESC_ESC = 0xdd;

    private final InputStream inputStream;
    private final int bufferLength;

    public SlipInputStream(final InputStream inputStream,
                           final int bufferLength) {
        this.inputStream = inputStream;
        this.bufferLength = bufferLength;
    }

    public SlipInputStream(final InputStream inputStream) {
        this(inputStream, DEFAULT_BUFFER_LENGTH);
    }

    public void receive(final PacketHandler handler) throws IOException, PacketHandlerException {
        byte[] buffer = new byte[bufferLength];
        int b;

        while (SLIP_END != (b = inputStream.read())) {
            if (-1 == b) return;
        }

        int i = 0;
        while (-1 != (b = inputStream.read())) {
            if (SLIP_END == b) {
                // the check for i>0 allows for SLIP variants in which packets both begin and end with SLIP_END
                if (i > 0) {
                    try {
                        handler.handle(buffer, i);
                    } catch (Exception e) {
                        throw new PacketHandlerException(e);
                    }
                }
                i = 0;
            } else {
                if (bufferLength == i) {
                    throw new IOException("buffer size of " + bufferLength + " exceeded");
                }

                if (SLIP_ESC == b) {
                    b = inputStream.read();
                    if (-1 == b) {
                        break;
                    }

                    if (SLIP_ESC_END == b) {
                        buffer[i++] = (byte) SLIP_END;
                    } else if (SLIP_ESC_ESC == b) {
                        buffer[i++] = (byte) SLIP_ESC;
                    } else {
                        throw new IOException("illegal escape sequence: found byte " + b + " after SLIP_ESC");
                    }
                } else {
                    buffer[i++] = (byte) b;
                }
            }
        }
    }

    public interface PacketHandler {
        void handle(byte[] buffer, int length);
    }

    public static class PacketHandlerException extends Exception {
        private PacketHandlerException(final Throwable cause) {
            super(cause);
        }
    }
}
