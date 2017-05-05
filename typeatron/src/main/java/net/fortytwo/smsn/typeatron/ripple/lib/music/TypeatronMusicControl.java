package net.fortytwo.smsn.typeatron.ripple.lib.music;

import com.illposed.osc.OSCMessage;
import net.fortytwo.smsn.SemanticSynchrony;
import net.fortytwo.smsn.config.Service;

import java.io.IOException;
import java.net.*;
import java.util.logging.Level;
import java.util.logging.Logger;

public class TypeatronMusicControl {
    private final Logger logger = SemanticSynchrony.getLogger(TypeatronMusicControl.class);

    private static final String
            MUSIC_CONTROL_ADDRESS = "net.fortytwo.smsn.typeatron.musicControlAddress",
            MUSIC_CONTROL_PORT = "net.fortytwo.smsn.typeatron.musicControlPort";

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

    private void sendMessage(final int key, final boolean pressedVsReleased) {
        int note = 5 - key;
        String address = "/exo/tt/note/" + note + "/" + (pressedVsReleased ? "on" : "off");
        OSCMessage m = new OSCMessage(address);

        try {

            byte[] buffer = m.getByteArray();
            DatagramPacket packet
                    = new DatagramPacket(buffer, buffer.length, musicControlAddress, musicControlPort);
            musicOscSocket.send(packet);

            // TODO: temporary
            logger.log(Level.INFO, "sent music control OSC datagram to " + musicControlAddress + ":" + musicControlPort);
        } catch (IOException e) {
            logger.log(Level.SEVERE, "error in sending OSC datagram to coordinator", e);
        } catch (Throwable t) {
            logger.log(Level.SEVERE, "unexpected error in sending OSC datagram to coordinator", t);
        }
    }
}
