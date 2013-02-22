package net.fortytwo.droidspeak;

/**
 * @author Joshua Shinavier (http://fortytwo.net)
 */
public class Voice {
    private double volume;
    private double[] harmonics;

    public Voice() {
        this.volume = 1.0;
        this.harmonics = new double[] {1.0};
    }

    public void setVolume(final Double volume) {
        if (volume <= 0) {
            throw new IllegalArgumentException("volume must be a positive number");
        }

        this.volume = volume;
    }

    public void setHarmonics(double... harmonics) {
        this.harmonics = harmonics;

        double sum = 0;
        for (double d : harmonics) {
            if (d < 0) {
                throw new IllegalArgumentException("negative values for harmonics are not allowed");
            }

            sum += d;
        }

        if (0 == sum) {
            throw new IllegalArgumentException("at least one non-zero harmonic is required");
        }

        for (int i = 0; i < harmonics.length; i++) {
            harmonics[i] /= sum;
        }
    }

    public double[] getHarmonics() {
        return harmonics;
    }

    public double getVolume() {
        return volume;
    }
}
