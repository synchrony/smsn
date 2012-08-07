package net.fortytwo.droidspeak;

/**
 * @author Joshua Shinavier (http://fortytwo.net)
 */
class SawGlide extends Syllable {
    private final double startFrequency;
    private final double endFrequency;

    public SawGlide(final long duration,
                    final double startFrequency,
                    final double endFrequency) {
        super(duration);
        this.startFrequency = startFrequency;
        this.endFrequency = endFrequency;
    }

    protected double getSample(final double time,
                               final Voice voice) {
        double freq = startFrequency +
                time * (endFrequency - startFrequency) / duration;

        double sum = 0;

        int i = 1;
        for (double d : voice.getHarmonics()) {
            double wl = 1 / (i * freq);
            double m = (time % wl) / wl;
            sum += d * (m < 0.5 ? -1 : 1);
            i++;
        }

        return sum * voice.getVolume();
    }
}
