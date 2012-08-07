package net.fortytwo.droidspeak;

/**
 * @author Joshua Shinavier (http://fortytwo.net)
 */
class Rest extends Syllable {
   public Rest(final long duration) {
        super(duration);
    }

    protected double getSample(double time,
                               final Voice voice) {
        return 0;
    }
}
