package net.fortytwo.smsn.monitron.data;

import org.junit.Test;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertFalse;
import static org.junit.Assert.assertTrue;

public class DataTest {

    @Test
    public void sampleIntervalFields() {
        Data data = new Data();

        long begin = System.currentTimeMillis();
        long end = begin + 1000;

        data.setSampleIntervalBeginning(begin);
        data.setSampleIntervalEnd(end);

        assertEquals(begin, data.getSampleIntervalBeginning());
        assertEquals(end, data.getSampleIntervalEnd());
    }

    @Test
    public void totalMeasurementsField() {
        Data data = new Data();

        data.setTotalMeasurements(42);
        assertEquals(42, data.getTotalMeasurements());

        data.setTotalMeasurements(Long.MAX_VALUE);
        assertEquals(Long.MAX_VALUE, data.getTotalMeasurements());
    }
}
