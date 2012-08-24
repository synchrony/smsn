package net.fortytwo.extendo.monitron;

import com.illposed.osc.OSCListener;
import com.illposed.osc.OSCMessage;
import com.illposed.osc.utility.OSCPacketDispatcher;
import net.fortytwo.extendo.monitron.listeners.SystemErrorListener;
import net.fortytwo.extendo.monitron.listeners.SystemTimerListener;
import net.fortytwo.extendo.monitron.listeners.sensors.BarometerListener;
import net.fortytwo.extendo.monitron.listeners.sensors.ColorLightLevelSensorListener;
import net.fortytwo.extendo.monitron.listeners.sensors.HygrometerListener;
import net.fortytwo.extendo.monitron.listeners.sensors.LightLevelSensorListener;
import net.fortytwo.extendo.monitron.listeners.sensors.OpticalDustSensorListener;
import net.fortytwo.extendo.monitron.listeners.sensors.PassiveInfraredSensorListener;
import net.fortytwo.extendo.monitron.listeners.sensors.SoundLevelSensorListener;
import net.fortytwo.extendo.monitron.listeners.sensors.ThermometerListener;
import net.fortytwo.extendo.monitron.listeners.sensors.VibrationLevelSensorListener;
import net.fortytwo.extendo.ontologies.MonitronOntology;
import net.fortytwo.extendo.ontologies.Universe;
import org.apache.commons.cli.*;

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

    private final InputStream input;
    private final OSCPacketDispatcher dispatcher;

    private boolean stopped = false;

    public MonitronService(final InputStream input) {
        this.input = input;

        dispatcher = new OSCPacketDispatcher();

        MonitronEventHandler c = new MonitronEventHandler();

        OSCListener errorListener = new SystemErrorListener(c);

        dispatcher.addListener(Universe.OM_SENSOR_7BB206L0_VIBRN,
                new VibrationLevelSensorListener(c, Universe.MURATA_7BB_20_6L0_1));
        dispatcher.addListener(Universe.OM_SENSOR_ADJDS311CR999_BLUE,
                new ColorLightLevelSensorListener(c, Universe.AVAGO_ADJD_S311_CR999_1, MonitronOntology.BLUE_LIGHT_LEVEL));
        dispatcher.addListener(Universe.OM_SENSOR_ADJDS311CR999_GREEN,
                new ColorLightLevelSensorListener(c, Universe.AVAGO_ADJD_S311_CR999_1, MonitronOntology.GREEN_LIGHT_LEVEL));
        dispatcher.addListener(Universe.OM_SENSOR_ADJDS311CR999_RED,
                new ColorLightLevelSensorListener(c, Universe.AVAGO_ADJD_S311_CR999_1, MonitronOntology.RED_LIGHT_LEVEL));
        dispatcher.addListener(Universe.OM_SENSOR_BMP085_PRESSURE,
                new BarometerListener(c, Universe.BOSCH_BMP085_1_BAROMETER));
        dispatcher.addListener(Universe.OM_SENSOR_BMP085_TEMP,
                new ThermometerListener(c, Universe.BOSCH_BMP085_1_THERMOMETER));
        dispatcher.addListener(Universe.OM_SENSOR_GP2Y1010AU0F_DUST,
                new OpticalDustSensorListener(c, Universe.SHARP_GP2Y101AU0F_1));
        dispatcher.addListener(Universe.OM_SENSOR_MD9745APZF_SOUND,
                new SoundLevelSensorListener(c, Universe.KNOWLES_MD9745APZ_F_1));
        dispatcher.addListener(Universe.OM_SENSOR_PHOTO_LIGHT,
                new LightLevelSensorListener(c, Universe.GENERIC_PHOTORESISTOR_1));
        dispatcher.addListener(Universe.OM_SENSOR_RHT03_ERROR,
                errorListener);  // TODO
        dispatcher.addListener(Universe.OM_SENSOR_RHT03_HUMID,
                new HygrometerListener(c, Universe.MAXDETECT_RHT03_1_HYGROMETER));
        dispatcher.addListener(Universe.OM_SENSOR_RHT03_TEMP,
                new ThermometerListener(c, Universe.MAXDETECT_RHT03_1_THERMOMETER));
        dispatcher.addListener(Universe.OM_SENSOR_SE10_MOTION,
                new PassiveInfraredSensorListener(c, Universe.HANSE_SE10_1));

        dispatcher.addListener(Universe.OM_SYSTEM_ERROR,
                errorListener);
        dispatcher.addListener(Universe.OM_SYSTEM_TIME,
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
            return new OSCMessage(address, Arrays.copyOfRange(parts, 1, parts.length));
        } else {
            return new OSCMessage(address);
        }
    }

    public static void main(final String[] args) throws ParseException {
        Options options = new Options();

        Option fileOpt = new Option("f", "file", true, "a file from which to load sensor data");
        fileOpt.setRequired(false);
        options.addOption(fileOpt);

        CommandLine cmd = new PosixParser().parse(options, args);
        String fileName = cmd.getOptionValue("file");

        try {
            InputStream input = null == fileName ? System.in : new FileInputStream(new File(fileName));
            MonitronService s = new MonitronService(input);
            s.run();
        } catch (Throwable t) {
            t.printStackTrace(System.err);
            System.exit(1);
        }
    }
}
