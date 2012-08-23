package net.fortytwo.extendo.monitron.data;

/**
 * @author Joshua Shinavier (http://fortytwo.net)
 */
public class MotionData extends Data {
    private boolean motionDetected;

    public boolean getMotionDetected() {
        return motionDetected;
    }

    public void setMotionDetected(boolean motionDetected) {
        this.motionDetected = motionDetected;
    }
}
