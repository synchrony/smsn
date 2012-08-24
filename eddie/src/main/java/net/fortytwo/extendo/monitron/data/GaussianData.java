package net.fortytwo.extendo.monitron.data;

/**
* @author Joshua Shinavier (http://fortytwo.net)
*/
public class GaussianData extends Data {

    private double minValue;
    private double maxValue;

    private double mean;
    private double variance;

    public double getMinValue() {
        return minValue;
    }

    public void setMinValue(double minValue) {
        this.minValue = minValue;
    }

    public double getMaxValue() {
        return maxValue;
    }

    public void setMaxValue(double maxValue) {
        this.maxValue = maxValue;
    }

    public double getMean() {
        return mean;
    }

    public void setMean(double mean) {
        this.mean = mean;
    }

    public double getVariance() {
        return variance;
    }

    public void setVariance(double variance) {
        this.variance = variance;
    }
}
