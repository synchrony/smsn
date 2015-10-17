package net.fortytwo.smsn.monitron;

import com.illposed.osc.AddressSelector;
import com.illposed.osc.OSCListener;
import com.illposed.osc.OSCMessage;
import com.illposed.osc.utility.JavaRegexAddressSelector;
import com.illposed.osc.utility.OSCPacketDispatcher;
import net.fortytwo.smsn.monitron.listeners.SystemErrorListener;
import net.fortytwo.smsn.monitron.listeners.SystemTimerListener;
import net.fortytwo.smsn.monitron.listeners.sensors.BarometerListener;
import net.fortytwo.smsn.monitron.listeners.sensors.ColorLightLevelSensorListener;
import net.fortytwo.smsn.monitron.listeners.sensors.HygrometerListener;
import net.fortytwo.smsn.monitron.listeners.sensors.LightLevelSensorListener;
import net.fortytwo.smsn.monitron.listeners.sensors.OpticalDustSensorListener;
import net.fortytwo.smsn.monitron.listeners.sensors.PassiveInfraredSensorListener;
import net.fortytwo.smsn.monitron.listeners.sensors.SoundLevelSensorListener;
import net.fortytwo.smsn.monitron.listeners.sensors.ThermometerListener;
import net.fortytwo.smsn.monitron.listeners.sensors.VibrationLevelSensorListener;
import net.fortytwo.smsn.monitron.ontologies.MonitronOntology;
import net.fortytwo.smsn.monitron.ontologies.Universe;

import java.io.BufferedReader;
import java.io.IOException;
import java.io.InputStream;
import java.io.InputStreamReader;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collection;
import java.util.Collections;
import java.util.LinkedList;

/*
sudo cu -l /dev/cu.usbserial-A600HFHJ -s 9600 | tee /tmp/arduino.out
 */

/**
 * @author Joshua Shinavier (http://fortytwo.net)
 */
public class MonitronService {

    private final InputStream input;
    private final OSCPacketDispatcher dispatcher;

    private boolean stopped = false;

    public MonitronService(final InputStream input,
                           final EventHandler handler) {
        this.input = input;

        dispatcher = new OSCPacketDispatcher();

        Context c = new Context(handler);

        OSCListener errorListener = new SystemErrorListener(c);

        dispatcher.addListener(new JavaRegexAddressSelector(Universe.OM_SENSOR_7BB206L0_VIBRN),
                new VibrationLevelSensorListener(c, Universe.MURATA_7BB_20_6L0_1));
        dispatcher.addListener(new JavaRegexAddressSelector(Universe.OM_SENSOR_ADJDS311CR999_BLUE),
                new ColorLightLevelSensorListener(
                        c, Universe.AVAGO_ADJD_S311_CR999_1, MonitronOntology.BLUE_LIGHT_LEVEL));
        dispatcher.addListener(new JavaRegexAddressSelector(Universe.OM_SENSOR_ADJDS311CR999_GREEN),
                new ColorLightLevelSensorListener(
                        c, Universe.AVAGO_ADJD_S311_CR999_1, MonitronOntology.GREEN_LIGHT_LEVEL));
        dispatcher.addListener(new JavaRegexAddressSelector(Universe.OM_SENSOR_ADJDS311CR999_RED),
                new ColorLightLevelSensorListener(
                        c, Universe.AVAGO_ADJD_S311_CR999_1, MonitronOntology.RED_LIGHT_LEVEL));
        dispatcher.addListener(new JavaRegexAddressSelector(Universe.OM_SENSOR_BMP085_PRESSURE),
                new BarometerListener(c, Universe.BOSCH_BMP085_1_BAROMETER));
        dispatcher.addListener(new JavaRegexAddressSelector(Universe.OM_SENSOR_BMP085_TEMP),
                new ThermometerListener(c, Universe.BOSCH_BMP085_1_THERMOMETER));
        dispatcher.addListener(new JavaRegexAddressSelector(Universe.OM_SENSOR_GP2Y1010AU0F_DUST),
                new OpticalDustSensorListener(c, Universe.SHARP_GP2Y101AU0F_1));
        dispatcher.addListener(new JavaRegexAddressSelector(Universe.OM_SENSOR_MD9745APZF_SOUND),
                new SoundLevelSensorListener(c, Universe.KNOWLES_MD9745APZ_F_1));
        dispatcher.addListener(new JavaRegexAddressSelector(Universe.OM_SENSOR_PHOTO_LIGHT),
                new LightLevelSensorListener(c, Universe.GENERIC_PHOTORESISTOR_1));
        dispatcher.addListener(new JavaRegexAddressSelector(Universe.OM_SENSOR_RHT03_ERROR),
                errorListener);  // TODO
        dispatcher.addListener(new JavaRegexAddressSelector(Universe.OM_SENSOR_RHT03_HUMID),
                new HygrometerListener(c, Universe.MAXDETECT_RHT03_1_HYGROMETER));
        dispatcher.addListener(new JavaRegexAddressSelector(Universe.OM_SENSOR_RHT03_TEMP),
                new ThermometerListener(c, Universe.MAXDETECT_RHT03_1_THERMOMETER));
        dispatcher.addListener(new JavaRegexAddressSelector(Universe.OM_SENSOR_SE10_MOTION),
                new PassiveInfraredSensorListener(c, Universe.HANSE_SE10_1));

        dispatcher.addListener(new JavaRegexAddressSelector(Universe.OM_SYSTEM_ERROR),
                errorListener);
        dispatcher.addListener(new JavaRegexAddressSelector(Universe.OM_SYSTEM_TIME),
                new SystemTimerListener(c));
    }

    public void run() throws IOException {
        stopped = false;

        BufferedReader br = new BufferedReader(new InputStreamReader(input));
        try {
            String line;
            while (!stopped && null != (line = br.readLine())) {
                OSCMessage m = parsePseudoOSCMessage(line.trim());

                if (null != m) {
                    dispatcher.dispatchPacket(m);
                }
            }
        } finally {
            input.close();
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
            Collection<Object> args = new ArrayList<Object>();
            args.addAll(Arrays.asList(parts).subList(1, parts.length));
            return new OSCMessage(address, args);
        } else {
            return new OSCMessage(address);
        }
    }
}
