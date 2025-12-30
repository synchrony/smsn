package net.fortytwo.smsn.monitron.data;

import org.junit.Test;

import static org.junit.Assert.assertEquals;

public class GaussianDataTest {

    private static final double DELTA = 0.0001;

    @Test
    public void minMaxValues() {
        GaussianData data = new GaussianData();

        data.setMinValue(-10.5);
        data.setMaxValue(100.5);

        assertEquals(-10.5, data.getMinValue(), DELTA);
        assertEquals(100.5, data.getMaxValue(), DELTA);
    }

    @Test
    public void meanAndStandardDeviation() {
        GaussianData data = new GaussianData();

        data.setMean(25.5);
        data.setStandardDeviation(3.14159);

        assertEquals(25.5, data.getMean(), DELTA);
        assertEquals(3.14159, data.getStandardDeviation(), DELTA);
    }

    @Test
    public void inheritsFromData() {
        GaussianData data = new GaussianData();

        // Verify inheritance from Data class
        long now = System.currentTimeMillis();
        data.setSampleIntervalBeginning(now);
        data.setSampleIntervalEnd(now + 1000);
        data.setTotalMeasurements(100);

        assertEquals(now, data.getSampleIntervalBeginning());
        assertEquals(now + 1000, data.getSampleIntervalEnd());
        assertEquals(100, data.getTotalMeasurements());
    }

    @Test
    public void edgeCases() {
        GaussianData data = new GaussianData();

        // Zero values
        data.setMean(0.0);
        data.setStandardDeviation(0.0);
        assertEquals(0.0, data.getMean(), DELTA);
        assertEquals(0.0, data.getStandardDeviation(), DELTA);

        // Very large values
        data.setMean(Double.MAX_VALUE);
        assertEquals(Double.MAX_VALUE, data.getMean(), DELTA);

        // Very small values
        data.setMean(Double.MIN_VALUE);
        assertEquals(Double.MIN_VALUE, data.getMean(), DELTA);

        // Negative values (for temperature readings, etc.)
        data.setMean(-273.15);  // Absolute zero in Celsius
        assertEquals(-273.15, data.getMean(), DELTA);
    }
}
