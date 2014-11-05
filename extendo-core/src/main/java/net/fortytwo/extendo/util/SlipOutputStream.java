package net.fortytwo.extendo.util;

import java.io.IOException;
import java.io.OutputStream;

/**
 * A utility for sending byte array packets via the Serial Line Internet Protocol (SLIP)
 *
 * @author Joshua Shinavier (http://fortytwo.net)
 */
public class SlipOutputStream {

    // a somewhat arbitrary UDP-inspired default
    private static final int DEFAULT_BUFFER_LENGTH = 1500;

    private static final int
            SLIP_END = 0xc0,
            SLIP_ESC = 0xdb,
            SLIP_ESC_END = 0xdc,
            SLIP_ESC_ESC = 0xdd;

    private final OutputStream outputStream;
    private final int bufferLength;

    public SlipOutputStream(final OutputStream outputStream,
                            final int bufferLength) {
        this.outputStream = outputStream;
        this.bufferLength = bufferLength;
    }

    public SlipOutputStream(final OutputStream outputStream) {
        this(outputStream, DEFAULT_BUFFER_LENGTH);
    }

    public void send(final byte[] packet) throws IOException {
        //out.write((byte) SLIP_END);
        for (byte b : packet) {
            if ((byte) SLIP_END == b) {
                outputStream.write((byte) SLIP_ESC);
                outputStream.write((byte) SLIP_ESC_END);
            } else if ((byte) SLIP_ESC == b) {
                outputStream.write((byte) SLIP_ESC);
                outputStream.write((byte) SLIP_ESC_ESC);
            }
        }
        outputStream.write((byte) SLIP_END);
    }
}
