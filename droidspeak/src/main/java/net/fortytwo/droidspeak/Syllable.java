package net.fortytwo.droidspeak;

import java.io.IOException;
import java.io.OutputStream;

/**
 * @author Joshua Shinavier (http://fortytwo.net)
 */
public abstract class Syllable {
    protected final double duration;

    Syllable(long duration) {
        this.duration = duration / 1000.0;
    }

    protected abstract double getSample(double time,
                                        Voice voice);

    public void play(final OutputStream out,
                     final Voice voice) throws IOException {
        long n = (long) (duration * Droidspeak.SAMPLE_RATE);

        for (int i = 0; i < n; i++) {
            double time = i / (1.0 * Droidspeak.SAMPLE_RATE);

            double sample = getSample(time, voice);

            writeSample(sample, out);
        }
    }

    protected void writeSample(final double sample,
                               final OutputStream out) throws IOException {
        short s = (short) (16000 * sample);
        out.write((byte) ((s >> 8) & 0xff));
        out.write((byte) (s & 0xff));
    }
}
