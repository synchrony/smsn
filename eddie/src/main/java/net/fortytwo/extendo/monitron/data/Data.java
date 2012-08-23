package net.fortytwo.extendo.monitron.data;

/**
 * @author Joshua Shinavier (http://fortytwo.net)
 */
public class Data {
    private long startTime;
    private long endTime;

    private long totalMeasurements;

    public long getStartTime() {
        return startTime;
    }

    public void setStartTime(long startTime) {
        this.startTime = startTime;
    }

    public long getEndTime() {
        return endTime;
    }

    public void setEndTime(long endTime) {
        this.endTime = endTime;
    }

    public long getTotalMeasurements() {
        return totalMeasurements;
    }

    public void setTotalMeasurements(long totalMeasurements) {
        this.totalMeasurements = totalMeasurements;
    }
}
