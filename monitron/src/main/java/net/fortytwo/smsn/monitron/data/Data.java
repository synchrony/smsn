package net.fortytwo.smsn.monitron.data;

/**
 * @author Joshua Shinavier (http://fortytwo.net)
 */
public class Data {
    private long sampleIntervalBeginning;
    private long sampleIntervalEnd;

    private long totalMeasurements;

    public long getSampleIntervalBeginning() {
        return sampleIntervalBeginning;
    }

    public void setSampleIntervalBeginning(long time) {
        this.sampleIntervalBeginning = time;
    }

    public long getSampleIntervalEnd() {
        return sampleIntervalEnd;
    }

    public void setSampleIntervalEnd(long time) {
        this.sampleIntervalEnd = time;
    }

    public long getTotalMeasurements() {
        return totalMeasurements;
    }

    public void setTotalMeasurements(long totalMeasurements) {
        this.totalMeasurements = totalMeasurements;
    }
}
