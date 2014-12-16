package net.fortytwo.extendo.server.gesture;

import java.util.HashMap;
import java.util.Map;

/**
 * @author Joshua Shinavier (http://fortytwo.net)
 */
public class GestureLowPassFilter {

    private final Map<String, Long> lastGestures;
    private final long minPeriod;

    public GestureLowPassFilter(final long minPeriod) {
        this.minPeriod = minPeriod;
        this.lastGestures = new HashMap<String, Long>();
    }

    public boolean doAllow( String actor1, String actor2, final long timestamp) {
        if (actor1.compareTo(actor2) > 0) {
            String tmp = actor1;
            actor1 = actor2;
            actor2 = tmp;
        }

        String key = actor1 + actor2;
        Long lastGesture = lastGestures.get(key);
        boolean allow = null == lastGesture || timestamp - lastGesture > minPeriod;

        // only store the given gesture as the last if it is not filtered out
        // That is, do not treat sequences of gestures as a single gesture
        if (allow) {
            lastGestures.put(key, timestamp);
        }

        return allow;
    }
}
