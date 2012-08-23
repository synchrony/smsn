package net.fortytwo.extendo.monitron.data;

/**
 * @author Joshua Shinavier (http://fortytwo.net)
 */
public class IntensityData extends Data {
    private double overallIntensity;

    private double minIntensity;
    private double maxIntensity;

    public double getOverallIntensity() {
        return overallIntensity;
    }

    public void setOverallIntensity(double overallIntensity) {
        this.overallIntensity = overallIntensity;
    }

    public double getMinIntensity() {
        return minIntensity;
    }

    public void setMinIntensity(double minIntensity) {
        this.minIntensity = minIntensity;
    }

    public double getMaxIntensity() {
        return maxIntensity;
    }

    public void setMaxIntensity(double maxIntensity) {
        this.maxIntensity = maxIntensity;
    }
}
