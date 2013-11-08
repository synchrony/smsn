package net.fortytwo.extendo.brainstem;

import com.illposed.osc.OSCMessage;
import com.illposed.osc.OSCPacket;
import com.illposed.osc.utility.OSCByteArrayToJavaConverter;

import java.io.File;
import java.util.logging.Logger;

/**
 * @author Joshua Shinavier (http://fortytwo.net)
 */
public class Brainstem {
    private static final Logger LOGGER = Logger.getLogger(Brainstem.class.getName());

    private static final String PROPS_PATH = "/sdcard/brainstem.props";

    public Brainstem() {

    }

    // note: in Android, SharedPreferences are preferred to properties files.  This file specifically contains those
    // settings which change more frequently than the APK is loaded, such as network settings.
    // Ideally, this file will go away entirely once the Brainstem becomes reusable software rather than a
    // special-purpose component of a demo.
    private void loadConfiguration() throws BrainstemException {
        File conf = new File(PROPS_PATH);
        if (!conf.exists()) {
            throw new BrainstemException("configuration properties not found: " + PROPS_PATH);
        }


    }

    public void addComplexEventHandler(final String query,
                                       final ComplexEventHandler handler) {
        // TODO
        // handle various continuous SPARQL results from facilitator
    }

    private void receiveOSCMessage(final String data,
                                   final OSCMessageHandler handler) {
        OSCByteArrayToJavaConverter c = new OSCByteArrayToJavaConverter();

        // TODO: is the array length all that is expected for the second argument?
        OSCPacket p = c.convert(data.getBytes(), data.getBytes().length);

        if (p instanceof OSCMessage) {
            handler.handle((OSCMessage) p);
        } else {
            LOGGER.warning("OSC packet is of non-message type " + p.getClass().getSimpleName() + ": " + p);
        }
    }

    public class BrainstemException extends Exception {
        public BrainstemException(final String message) {
            super(message);
        }
    }
}
