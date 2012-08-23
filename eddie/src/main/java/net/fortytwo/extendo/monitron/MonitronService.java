package net.fortytwo.extendo.monitron;

import com.illposed.osc.OSCListener;
import com.illposed.osc.OSCMessage;
import com.illposed.osc.utility.OSCPacketDispatcher;
import net.fortytwo.extendo.monitron.listeners.MicrophoneListener;
import net.fortytwo.extendo.monitron.listeners.GenericErrorListener;

import java.io.BufferedReader;
import java.io.File;
import java.io.FileInputStream;
import java.io.IOException;
import java.io.InputStream;
import java.io.InputStreamReader;
import java.util.Arrays;

/*
sudo cu -l /dev/cu.usbserial-A600HFHJ -s 9600 | tee /tmp/arduino.out
 */

/**
 * @author Joshua Shinavier (http://fortytwo.net)
 */
public class MonitronService {

    public static final String
            AUDIO_OSC_PREFIX = "/om/sensor/phone",
            BMP085_OSC_PREFIX = "/om/sensor/BMP085",
            DHT22_OSC_PREFIX = "/om/sensor/dht22",
            DUST_OSC_PREFIX = "/om/sensor/dust",
            PHOTO_OSC_PREFIX = "/om/sensor/photo",
            TIMER_OSC_PREFIX = "/om/timer",
            VIBRO_OSC_PREFIX = "/om/sensor/piezo";

    private static final String
            DATA_OSC_SUFFIX = "/data",
            ERROR_OSC_SUFFIX = "/error";

    private final File inputFile;
    private final OSCPacketDispatcher dispatcher;

    private boolean stopped = false;

    public MonitronService(final File inputFile) {
        this.inputFile = inputFile;

        dispatcher = new OSCPacketDispatcher();

        OSCListener errorListener = new GenericErrorListener();

        dispatcher.addListener(AUDIO_OSC_PREFIX + DATA_OSC_SUFFIX, new MicrophoneListener());
        dispatcher.addListener(AUDIO_OSC_PREFIX + ERROR_OSC_SUFFIX, errorListener);

    }

    public void run() throws IOException {
        stopped = false;

        InputStream is = new FileInputStream(inputFile);
        BufferedReader br = new BufferedReader(new InputStreamReader(is));
        try {
            String line;
            while (!stopped && null != (line = br.readLine())) {
                OSCMessage m = parsePseudoOSCMessage(line.trim());

                if (null != m) {
                    dispatcher.dispatchPacket(m);
                }
            }
        } finally {
            is.close();
        }
    }

    public void stop() {
        stopped = true;
    }

    private OSCMessage parsePseudoOSCMessage(final String s) {
        if (!s.startsWith("/")) {
            return null;
        }

        String[] parts = s.split(" ");

        String address = parts[0];
        if (parts.length > 1) {
            return new OSCMessage(address, Arrays.copyOfRange(parts, 1, parts.length));
        } else {
            return new OSCMessage(address);
        }
    }
}
