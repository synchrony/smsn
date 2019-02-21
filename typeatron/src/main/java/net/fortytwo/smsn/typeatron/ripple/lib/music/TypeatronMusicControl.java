package net.fortytwo.smsn.typeatron.ripple.lib.music;

import com.illposed.osc.OSCMessage;
import com.illposed.osc.OSCSerializer;
import com.illposed.osc.OSCSerializerFactory;
import net.fortytwo.smsn.SemanticSynchrony;
import net.fortytwo.smsn.config.Service;

import java.io.IOException;
import java.net.*;
import java.nio.ByteBuffer;
import java.util.logging.Level;
import java.util.logging.Logger;

public class TypeatronMusicControl {

    private final InetAddress musicControlAddress;
    private final int musicControlPort;
    private final DatagramSocket musicOscSocket;

    private boolean enabled;

    public TypeatronMusicControl() throws SocketException, UnknownHostException {
        Service conf = SemanticSynchrony.getConfiguration().getServices().getMusic();
        String s = conf.getHost();
        musicControlAddress = null == s ? null : InetAddress.getByName(s);
        musicControlPort = conf.getPort();

        if (null != musicControlAddress && musicControlPort > 0) {
            musicOscSocket = new DatagramSocket();
        } else {
            musicOscSocket = null;
        }
    }

    public void enable() {
        if (null != musicOscSocket) {
            enabled = true;
        }
    }

    public void disable() {
        enabled = false;
    }

    public void handleKeyPressed(int key) {
        if (enabled) {
            sendMessage(key, true);
        }
    }

    public void handleKeyReleased(int key) {
        if (enabled) {
            sendMessage(key, false);
        }
    }

    private static final int BUFFER_SIZE = 10 * 1024;
    private static final ThreadLocal<ByteBuffer> messageContents = ThreadLocal.withInitial(() -> ByteBuffer.allocate(BUFFER_SIZE));
    private static final OSCSerializerFactory oscSerializerFactory = OSCSerializerFactory.createDefaultFactory();
	// HACK The above fields and the method below are duplicated in: (module:) smsn-services - (file:) net/fortytwo/smsn/p2p/SmSnAgent.java
    public static void sendOSCMessage(final DatagramSocket datagramSocket, final InetAddress address, final int port, final OSCMessage m, final Logger logger) {

        try {
            messageContents.get().rewind();
            final OSCSerializer oscSerializer = oscSerializerFactory.create(messageContents.get());
            oscSerializer.write(m);
            if (messageContents.get().position() >= (BUFFER_SIZE - 1)) {
                logger.log(Level.WARNING, "message length (" + messageContents.get().position()
                        + " bytes) should be kept under the buffer capacity (${BUFFER_SIZE} bytes)");
            }

            messageContents.get().flip();
            DatagramPacket packet = new DatagramPacket(messageContents.get().array(), messageContents.get().limit(), address, port);
            datagramSocket.send(packet);

            logger.log(Level.INFO, "sent OSC datagram to " + address + ":" + port);
        } catch (IOException e) {
            logger.log(Level.SEVERE, "error in sending OSC datagram to coordinator", e);
        } catch (Throwable t) {
            logger.log(Level.SEVERE, "unexpected error in sending OSC datagram to coordinator", t);
        }
    }

    private void sendMessage(final int key, final boolean pressedVsReleased) {
        int note = 5 - key;
        String address = "/exo/tt/note/" + note + "/" + (pressedVsReleased ? "on" : "off");
        OSCMessage m = new OSCMessage(address);

        sendOSCMessage(musicOscSocket, musicControlAddress, musicControlPort, m, SemanticSynchrony.getLogger());
    }
}
