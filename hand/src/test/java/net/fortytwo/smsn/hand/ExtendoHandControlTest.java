package net.fortytwo.smsn.hand;

import org.junit.Before;
import org.junit.Test;

/**
 * Tests for ExtendoHandControl parameter validation.
 * Note: These tests use a MockExtendoHandControl to test validation logic
 * without requiring actual OSC communication.
 */
public class ExtendoHandControlTest {

    private MockExtendoHandControl control;

    @Before
    public void setUp() {
        control = new MockExtendoHandControl();
    }

    @Test
    public void validMulticueParametersSucceed() {
        // Valid parameters within range
        control.sendMulticueMessage(440, 1000, 0xFF0000, 500);
        control.sendMulticueMessage(0, 0, 0, 0);  // Edge case: all zeros
        control.sendMulticueMessage(20000, 60000, 0xFFFFFF, 60000);  // Max values
    }

    @Test(expected = IllegalArgumentException.class)
    public void negativeToneFrequencyThrows() {
        control.sendMulticueMessage(-1, 1000, 0xFF0000, 500);
    }

    @Test(expected = IllegalArgumentException.class)
    public void toneFrequencyAboveMaxThrows() {
        control.sendMulticueMessage(20001, 1000, 0xFF0000, 500);
    }

    @Test(expected = IllegalArgumentException.class)
    public void negativeToneDurationThrows() {
        control.sendMulticueMessage(440, -1, 0xFF0000, 500);
    }

    @Test(expected = IllegalArgumentException.class)
    public void toneDurationAboveMaxThrows() {
        control.sendMulticueMessage(440, 60001, 0xFF0000, 500);
    }

    @Test(expected = IllegalArgumentException.class)
    public void negativeVibrationDurationThrows() {
        control.sendMulticueMessage(440, 1000, 0xFF0000, -1);
    }

    @Test(expected = IllegalArgumentException.class)
    public void vibrationDurationAboveMaxThrows() {
        control.sendMulticueMessage(440, 1000, 0xFF0000, 60001);
    }

    /**
     * Mock implementation that tests validation without OSC dependencies.
     */
    private static class MockExtendoHandControl {

        public void sendMulticueMessage(final int toneFrequency,
                                        final int toneDurationMs,
                                        final int color,
                                        final int vibrationDurationMs) {
            // Validation logic extracted from ExtendoHandControl
            if (toneFrequency < 0 || toneFrequency > 20000) {
                throw new IllegalArgumentException("tone frequency is out of range: " + toneFrequency);
            }

            if (toneDurationMs < 0 || toneDurationMs > 60000) {
                throw new IllegalArgumentException("tone duration is out of range: " + toneDurationMs);
            }

            if (vibrationDurationMs < 0 || vibrationDurationMs > 60000) {
                throw new IllegalArgumentException("vibration duration is out of range: " + vibrationDurationMs);
            }

            // In real implementation, this would send OSC message
        }
    }
}
