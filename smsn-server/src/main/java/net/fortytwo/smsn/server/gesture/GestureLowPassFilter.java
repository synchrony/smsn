package net.fortytwo.smsn.server.gesture;

import org.openrdf.model.URI;

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

    public boolean doAllow(URI actor1, URI actor2, final long timestamp) {
        String a1 = actor1.stringValue();
        String a2 = actor2.stringValue();

        if (a1.compareTo(a2) > 0) {
            String tmp = a1;
            a1 = a2;
            a2 = tmp;
        }

        String key = a1 + a2;
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
