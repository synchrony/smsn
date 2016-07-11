package net.fortytwo.smsn.util.slip;

import java.io.IOException;
import java.io.OutputStream;
import java.util.concurrent.LinkedBlockingQueue;
import java.util.logging.Level;
import java.util.logging.Logger;

/**
 * A utility for sending byte array packets via the Serial Line Internet Protocol (SLIP)
 *
 * @author Joshua Shinavier (http://fortytwo.net)
 */
public class SlipOutputStream {
    private static final Logger logger = Logger.getLogger(SlipOutputStream.class.getName());

    private static final int
            SLIP_END = 0xc0,
            SLIP_ESC = 0xdb,
            SLIP_ESC_END = 0xdc,
            SLIP_ESC_ESC = 0xdd;

    private final OutputStream outputStream;

    private LinkedBlockingQueue<byte[]> queue;
    private Thread thread;

    private long throttlingPeriod;
    private long timeOfLastPacket;

    /**
     * Constructs a new SlipOutputStream, optionally with an internal thread
     *
     * @param outputStream the lower-level stream to which to write packet data
     * @param threaded     if true, this stream will synchronize outbound serial communications
     *                     using an internal thread
     */
    public SlipOutputStream(final OutputStream outputStream, final boolean threaded) {
        this.outputStream = outputStream;
        queue = threaded ? new LinkedBlockingQueue<>() : null;

        if (threaded) {
            thread = new Thread(new Runnable() {
                @Override
                public void run() {
                    long lastPacketSent = 0l;

                    try {
                        while (!Thread.currentThread().isInterrupted()) {
                            // blocks until a packet is available
                            byte[] packet = queue.take();

                            sendInternal(packet);
                        }
                    } catch (IOException e) {
                        logger.log(Level.SEVERE, "I/O exception while writing SLIP packet", e);
                    } catch (Throwable t) {
                        logger.log(Level.SEVERE, "unexpected error in ThreadedSlipOutputStream. Quitting thread", t);
                    }
                }
            });
            thread.start();
        }
    }

    /**
     * Enforces a delay between the sending of consecutive packets, necessary for downstream applications which
     * are sensitive to data rate
     *
     * @param throttlingPeriod the minimum time in milliseconds between the moment at which we begin to send one
     *                         packet and the moment at which we begin to send the next
     *                         Note: a throttling period is not necessary to ensure synchronous and sequential writing
     *                         of packets to the output stream (for that, use the threaded option)
     */
    public void setThrottlingPeriod(final long throttlingPeriod) {
        this.throttlingPeriod = throttlingPeriod;
    }

    /**
     * Constructs a new non-threaded SlipOutputStream
     *
     * @param outputStream the wrapped, transport-level stream
     */
    public SlipOutputStream(final OutputStream outputStream) {
        this(outputStream, false);
    }

    public void send(final byte[] packet) throws IOException {
        if (null == queue) {
            sendInternal(packet);
        } else {
            queue.add(packet);
        }
    }

    private void sendInternal(final byte[] packet) throws IOException {
        throttle();

        // Some SLIP implementations begin, as well as end, packets with END.
        // We choose not to do so here.
        // outputStream.write((byte) SLIP_END);

        for (byte b : packet) {
            if ((byte) SLIP_END == b) {
                outputStream.write((byte) SLIP_ESC);
                outputStream.write((byte) SLIP_ESC_END);
            } else if ((byte) SLIP_ESC == b) {
                outputStream.write((byte) SLIP_ESC);
                outputStream.write((byte) SLIP_ESC_ESC);
            } else {
                outputStream.write(b);
            }
        }
        outputStream.write((byte) SLIP_END);
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

    public void close() throws IOException {
        outputStream.close();

        if (null != queue) {
            queue = null;
            thread.interrupt();
        }
    }
}
