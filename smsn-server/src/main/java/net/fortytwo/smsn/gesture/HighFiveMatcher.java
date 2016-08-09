package net.fortytwo.smsn.gesture;

import org.openrdf.model.IRI;

public class HighFiveMatcher {

    // the maximum interval allowed between matching claps of a high-five
    // TODO: this figure has been chosen arbitrarily, and is not yet based on an analysis of sample data
    private static final long PEAK_MAX_GAP = 200;

    private Clap lastClap;

    private final HighFiveHandler handler;

    private final GestureLowPassFilter lowPassFilter;

    public HighFiveMatcher(HighFiveHandler handler) {
        this.handler = handler;
        lowPassFilter = new GestureLowPassFilter(5000);
    }

    public void reset() {
        lastClap = null;
    }

    private void cleanup(final long timestamp) {
        if (null != lastClap && timestamp - lastClap.timeOfPeak > PEAK_MAX_GAP) {
            lastClap = null;
        }
    }

    public synchronized void receiveEvent(final IRI actor,
                                          final long timestamp) {
        //System.out.println("received clap by " + actor + " at " + timestamp);
        cleanup(timestamp);

        Clap newClap = new Clap();
        newClap.actor = actor;
        newClap.timeOfPeak = timestamp;

        if (null == lastClap
                || !lastClap.matches(newClap)
                || !lowPassFilter.doAllow(lastClap.actor, newClap.actor, timestamp)) {
            lastClap = newClap;
        } else if (lastClap.matches(newClap)) {
            handler.handle(lastClap, newClap, timestamp);
            lastClap = null;
        }
    }

    public interface HighFiveHandler {
        void handle(Clap left, Clap right, long time);
    }

    public class Clap {
        public IRI actor;
        public long timeOfPeak;

        public boolean matches(final Clap other) {
            return !other.actor.equals(actor) && Math.abs(other.timeOfPeak - timeOfPeak) < PEAK_MAX_GAP;
        }
    }
}
