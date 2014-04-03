package net.fortytwo.extendo.droidspeak;

/**
 * @author Joshua Shinavier (http://fortytwo.net)
 */
class SineGlide extends Syllable {
    private final double startFrequency;
    private final double endFrequency;

    public SineGlide(final long duration,
                     final double startFrequency,
                     final double endFrequency) {
        super(duration);
        this.startFrequency = startFrequency;
        this.endFrequency = endFrequency;
    }

    protected double getSample(double time,
                               final Voice voice) {
        double freq = startFrequency +
                time * (endFrequency - startFrequency) / duration;
        return Math.sin(2 * Math.PI * freq * time);
    }
}
