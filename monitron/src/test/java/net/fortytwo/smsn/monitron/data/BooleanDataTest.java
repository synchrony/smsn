package net.fortytwo.smsn.monitron.data;

import org.junit.Test;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertFalse;
import static org.junit.Assert.assertTrue;

public class BooleanDataTest {

    @Test
    public void resultField() {
        BooleanData data = new BooleanData();

        // Default should be false
        assertFalse(data.getResult());

        data.setResult(true);
        assertTrue(data.getResult());

        data.setResult(false);
        assertFalse(data.getResult());
    }

    @Test
    public void inheritsFromData() {
        BooleanData data = new BooleanData();

        // Verify inheritance from Data class
        long now = System.currentTimeMillis();
        data.setSampleIntervalBeginning(now);
        data.setSampleIntervalEnd(now + 500);
        data.setTotalMeasurements(1);

        assertEquals(now, data.getSampleIntervalBeginning());
        assertEquals(now + 500, data.getSampleIntervalEnd());
        assertEquals(1, data.getTotalMeasurements());
    }
}
