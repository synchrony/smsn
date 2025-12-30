package net.fortytwo.smsn.gesture;

import org.eclipse.rdf4j.model.IRI;

import java.util.HashMap;
import java.util.Map;

public class GestureLowPassFilter {

    private final Map<String, Long> lastGestures;
    private final long minPeriod;

    public GestureLowPassFilter(final long minPeriod) {
        this.minPeriod = minPeriod;
        this.lastGestures = new HashMap<>();
    }

    public boolean doAllow(IRI actor1, IRI actor2, final long timestamp) {
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
