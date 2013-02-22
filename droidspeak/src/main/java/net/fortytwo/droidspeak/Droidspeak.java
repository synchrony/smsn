package net.fortytwo.droidspeak;

import javax.sound.sampled.AudioFormat;
import javax.sound.sampled.AudioSystem;
import javax.sound.sampled.LineUnavailableException;
import javax.sound.sampled.SourceDataLine;
import javax.swing.*;
import java.io.BufferedReader;
import java.io.IOException;
import java.io.InputStream;
import java.io.InputStreamReader;
import java.io.OutputStream;
import java.io.PipedInputStream;
import java.io.PipedOutputStream;

public class Droidspeak extends JFrame {

    private static final String PREFIX = "/eddie/droidspeak/";

    //Allowable 8000,11025,16000,22050,44100
    public static final int SAMPLE_RATE = 16000;

    //Allowable 8,16
    private static final int SAMPLE_SIZE_IN_BITS = 16;

    //Allowable 1,2
    private static final int CHANNELS = 1;

    //Allowable true,false
    private static final boolean SIGNED = true;

    //Allowable true, false
    private static final boolean BIG_ENDIAN = true;

    public static final AudioFormat AUDIO_FORMAT;

    static {
        //Get the required audio format
        AUDIO_FORMAT = new AudioFormat(
                SAMPLE_RATE,
                SAMPLE_SIZE_IN_BITS,
                CHANNELS,
                SIGNED,
                BIG_ENDIAN);
    }

    public Droidspeak() throws IOException, LineUnavailableException {//constructor
        //        InputStream in = Droidspeak.class.getResourceAsStream("ok.osc");
//        InputStream in = Droidspeak.class.getResourceAsStream("okeedokee.osc");
//        InputStream in = Droidspeak.class.getResourceAsStream("siren.osc");

        //speak(Droidspeak.class.getResourceAsStream("ok.osc"));
        //speak(Droidspeak.class.getResourceAsStream("okeedokee.osc"));
        speak(Droidspeak.class.getResourceAsStream("siren.osc"));
        //speak(Droidspeak.class.getResourceAsStream("wow.osc"));
    }

    void speak(final InputStream in) throws LineUnavailableException, IOException {
        BufferedReader r = new BufferedReader(new InputStreamReader(in));

        OutputStream out = createOutputStream();

        Voice v = new Voice();
        v.setHarmonics(1.0, 0.5, 0.25);
        //v.setVolume(0.5);

        String line;
        while (null != (line = r.readLine())) {
            line = line.trim();
            if (line.length() > 0) {
                Syllable s = parseSyllable(line);
                s.play(out, v);
            }
        }
    }

    private OutputStream createOutputStream() throws IOException, LineUnavailableException {
        PipedOutputStream out = new PipedOutputStream();
        PipedInputStream in = new PipedInputStream(out);

        SourceDataLine line = AudioSystem.getSourceDataLine(Droidspeak.AUDIO_FORMAT);

        new ListenThread(in, line).start();

        return out;
    }

    // TODO: regex and error checking
    private Syllable parseSyllable(String line) {
        line = line.substring(PREFIX.length());

        if (line.startsWith("rest")) {
            line = line.substring(5);
            long duration = new Long(line);

            return new Rest(duration);
        } else if (line.startsWith("tone/")) {
            line = line.substring(5);
            String[] args = line.split(" ");
            String type = args[0];
            long duration = new Long(args[1]);
            double frequency = new Double(args[2]);

            if (type.equals("saw")) {
                return new SawTone(duration, frequency);
            } else if (type.equals("sine")) {
                return new SineTone(duration, frequency);
            }
        } else if (line.startsWith("glide/")) {
            line = line.substring(6);
            String[] args = line.split(" ");
            String type = args[0];
            long duration = new Long(args[1]);
            double startFrequency = new Double(args[2]);
            double endFrequency = new Double(args[3]);

            if (type.equals("saw")) {
                return new SawGlide(duration, startFrequency, endFrequency);
            } else if (type.equals("sine")) {
                return new SineGlide(duration, startFrequency, endFrequency);
            }
        }

        throw new IllegalArgumentException();
    }

    public static void main(String args[]) throws IOException, LineUnavailableException {
        new Droidspeak();
    }
}
