package net.fortytwo.smsn.server.gesture;

import org.openrdf.model.URI;

import java.util.Collection;
import java.util.HashMap;
import java.util.LinkedList;
import java.util.List;
import java.util.Map;
import java.util.Stack;
import java.util.logging.Logger;

/**
 * @author Joshua Shinavier (http://fortytwo.net)
 */
public class HandshakeMatcher {
    private static final Logger logger = Logger.getLogger(HandshakeMatcher.class.getName());

    // should match the constant used by Extend-o-Hand for handshake peak recognition
    private static final long PEAK_MAX_GAP = 353;

    private static final int STACK_SIZE_WARN_THRESHOLD = 1000;
    private long lastWarning = 0;

    private final Map<URI, HandshakeSequence> handshakesByActor;
    private final Stack<HandshakeSequence> latestHandshakeSequences;
    private Collection<HandshakeSequence> cleanupBuffer = new LinkedList<HandshakeSequence>();

    private HandshakeHandler handler;

    private final GestureLowPassFilter lowPassFilter;

    public HandshakeMatcher(HandshakeHandler handler) {
        this.handler = handler;
        this.handshakesByActor = new HashMap<URI, HandshakeSequence>();
        this.latestHandshakeSequences = new Stack<HandshakeSequence>();
        lowPassFilter = new GestureLowPassFilter(5000);
    }

    public void setHandler(final HandshakeHandler handler) {
        this.handler = handler;
    }

    public void reset() {
        handshakesByActor.clear();
        latestHandshakeSequences.clear();
    }

    private boolean isOld(final HandshakeSequence h,
                          final long now) {
        return now - h.latestPeak > PEAK_MAX_GAP;
    }

    private void cleanup(final long timestamp) {
        for (HandshakeSequence h : latestHandshakeSequences) {
            if (isOld(h, timestamp)) {
                cleanupBuffer.add(h);
            }
        }

        if (cleanupBuffer.size() > 0) {
            for (HandshakeSequence h : cleanupBuffer) {
                latestHandshakeSequences.remove(h);
                handshakesByActor.remove(h.actor);
            }

            cleanupBuffer.clear();
        }
    }

    public synchronized void receiveEvent(final URI actor,
                                          final long timestamp,
                                          final long now) {
        //System.out.println("received handshake by " + actor + " at " + timestamp);
        cleanup(timestamp);

        HandshakeSequence gesture = handshakesByActor.get(actor);
        boolean isNew = false;

        if (null == gesture) {
            gesture = new HandshakeSequence();
            gesture.actor = actor;
            gesture.firstPeak = timestamp;
            handshakesByActor.put(gesture.actor, gesture);
            isNew = true;
        } else if (timestamp < gesture.latestPeak) {
            throw new IllegalStateException("handshake peaks of actor " + actor + " arrived out of order ("
                    + timestamp + "<=" + gesture.latestPeak + ")");
        }

        gesture.peaks.add(timestamp);
        gesture.latestPeak = timestamp;

        if (!gesture.matched) {
            for (HandshakeSequence h : latestHandshakeSequences) {
                if (h.matches(gesture)) {
                    // don't produce the gesture if it is redundant
                    if (lowPassFilter.doAllow(gesture.actor, h.actor, timestamp)) {
                        handler.handle(gesture, h, timestamp);
                    }

                    // mark both handshakes as matched and leave them in the stack,
                    // so we don't match them again
                    h.matched = true;
                    gesture.matched = true;

                    break;
                }
            }
        }

        if (isNew) {
            latestHandshakeSequences.add(gesture);
            if (latestHandshakeSequences.size() > STACK_SIZE_WARN_THRESHOLD) {
                if (now - lastWarning > 10000) {
                    logger.warning("gestural stack is suspiciously large");
                    lastWarning = now;
                }
            }

            /*System.out.println("current stack:");
            for (Handshake h : latestHandshakes) {
                System.out.println("\t" + h);
            }*/
        }
    }

    public interface HandshakeHandler {
        void handle(HandshakeSequence left, HandshakeSequence right, long time);
    }

    public class HandshakeSequence {
        public URI actor;
        public final List<Long> peaks = new LinkedList<Long>();
        public long firstPeak;
        public long latestPeak;
        private boolean matched = false;

        public boolean matches(HandshakeSequence other) {
            return !other.actor.equals(actor)
                    && !matched && !other.matched
                    && ((other.firstPeak >= firstPeak && other.firstPeak <= latestPeak
                    && latestPeak >= other.firstPeak && latestPeak <= other.latestPeak)
                    || (firstPeak >= other.firstPeak && firstPeak <= other.latestPeak
                    && other.latestPeak >= firstPeak && other.latestPeak <= latestPeak));
        }
    }
}
