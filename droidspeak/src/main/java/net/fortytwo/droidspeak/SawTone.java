package net.fortytwo.droidspeak;

/**
 * @author Joshua Shinavier (http://fortytwo.net)
 */
class SawTone extends Syllable {
    private final double waveLength;

    public SawTone(final long duration,
                   final double frequency) {
        super(duration);
        this.waveLength = 1 / frequency;
    }

    protected double getSample(double time,
                               final Voice voice) {
        double m = (time % waveLength) / waveLength;
        return m < 0.5 ? -1 : 1;
    }
}
