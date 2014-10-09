package net.fortytwo.extendo.util;

import java.io.IOException;
import java.io.InputStream;
import java.io.OutputStream;

/**
 * A utility for sending and receiving byte array packets via the Serial Line Internet Protocol (SLIP)
 *
 * @author Joshua Shinavier (http://fortytwo.net)
 */
public class SlipStream {

    // a somewhat arbitrary UDP-inspired default
    private static final int DEFAULT_BUFFER_LENGTH = 1500;

    private static final int
            SLIP_END = 0xc0,
            SLIP_ESC = 0xdb,
            SLIP_ESC_END = 0xdc,
            SLIP_ESC_ESC = 0xdd;

    private final int bufferLength;

    public SlipStream(final int bufferLength) {
        this.bufferLength = bufferLength;
    }

    public SlipStream() {
        this(DEFAULT_BUFFER_LENGTH);
    }

    public void send(final OutputStream out,
                     final byte[] packet) throws IOException {
        //out.write((byte) SLIP_END);
        for (byte b : packet) {
            if ((byte) SLIP_END == b) {
                out.write((byte) SLIP_ESC);
                out.write((byte) SLIP_ESC_END);
            } else if ((byte) SLIP_ESC == b) {
                out.write((byte) SLIP_ESC);
                out.write((byte) SLIP_ESC_ESC);
            }
        }
        out.write((byte) SLIP_END);
    }

    public void receive(final InputStream in,
                        final PacketHandler handler) throws IOException, PacketHandlerException {
        byte[] buffer = new byte[bufferLength];
        int b;

        while (SLIP_END != (b = in.read())) {
            if (-1 == b) return;
        }

        int i = 0;
        while (-1 != (b = in.read())) {
            if (SLIP_END == b) {
                // the check for i>0 allows for SLIP variants in which packets both begin and end with END
                if (i > 0) {
                    try {
                        handler.handle(buffer, i);
                    } catch (Exception e) {
                        throw new PacketHandlerException(e);
                    }
                }
                i = 0;
            } else if (SLIP_ESC == b) {
                b = in.read();
                if (-1 == b) break;

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

    public interface PacketHandler {
        void handle(byte[] buffer, int length) throws Exception;
    }

    public static class PacketHandlerException extends Exception {
        private PacketHandlerException(final Throwable cause) {
            super(cause);
        }
    }
}
