package net.fortytwo.extendo.typeatron.ripple.lib.music;

import com.illposed.osc.OSCMessage;
import net.fortytwo.extendo.Extendo;
import net.fortytwo.extendo.util.TypedProperties;

import java.io.IOException;
import java.net.DatagramPacket;
import java.net.DatagramSocket;
import java.net.InetAddress;
import java.net.SocketException;
import java.net.UnknownHostException;
import java.util.logging.Level;
import java.util.logging.Logger;

/**
 * @author Joshua Shinavier (http://fortytwo.net)
 */
public class TypeatronMusicControl {
    private final Logger logger = Extendo.getLogger(TypeatronMusicControl.class);

    private static final String
            MUSIC_CONTROL_ADDRESS = "net.fortytwo.extendo.typeatron.musicControlAddress",
            MUSIC_CONTROL_PORT = "net.fortytwo.extendo.typeatron.musicControlPort";

    private final InetAddress musicControlAddress;
    private final int musicControlPort;
    private final DatagramSocket musicOscSocket;

    private boolean enabled;

    public TypeatronMusicControl() throws TypedProperties.PropertyException, SocketException, UnknownHostException {
        TypedProperties conf = Extendo.getConfiguration();
        String s = conf.getString(MUSIC_CONTROL_ADDRESS, null);
        musicControlAddress = null == s ? null : InetAddress.getByName(s);
        musicControlPort = conf.getInt(MUSIC_CONTROL_PORT, 0);

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
            logger.log(Level.SEVERE, "error in sending OSC datagram to facilitator", e);
        } catch (Throwable t) {
            logger.log(Level.SEVERE, "unexpected error in sending OSC datagram to facilitator", t);
        }
    }
}
