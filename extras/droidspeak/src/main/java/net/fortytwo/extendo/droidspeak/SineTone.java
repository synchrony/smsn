package net.fortytwo.extendo.droidspeak;

/**
* @author Joshua Shinavier (http://fortytwo.net)
*/
class SineTone extends Syllable {
    private final double frequency;

    public SineTone(final long duration,
                    final double frequency) {
        super(duration);
        this.frequency = frequency;
    }

    protected double getSample(double time,
                               final Voice voice) {
        return Math.sin(2 * Math.PI * frequency * time);
    }
}
