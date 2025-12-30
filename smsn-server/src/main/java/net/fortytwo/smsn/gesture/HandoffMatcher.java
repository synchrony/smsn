package net.fortytwo.smsn.gesture;

import org.eclipse.rdf4j.model.IRI;

import java.util.Collection;
import java.util.HashMap;
import java.util.LinkedList;
import java.util.Map;
import java.util.Stack;
import java.util.logging.Logger;
import java.util.stream.Collectors;

public class HandoffMatcher {
    private static final Logger logger = Logger.getLogger(HandoffMatcher.class.getName());

    // This should be an overestimate.  give/take spikes are assumed logically to occur at the same moment, but
    // take 30-40ms from trough to trough.  They can be recognized at any time during the first 15-20ms, which
    // contributes 20ms of potential offset.
    // Serial and network latency are assumed to be around the same for both devices, but we allow another 40ms
    // of difference, with Bluetooth latency being the greatest source of variability (standard deviations of
    // just over 40ms in serial-over-Bluetooth latency of a single connection have been measured using the current
    // Arduino+Bluetooth setup, although this setup needs to be improved).
    private static final long SPIKE_MAX_GAP = 60;

    private static final int STACK_SIZE_WARN_THRESHOLD = 1000;
    private long lastWarning = 0;

    private final Stack<Handoff> latestHandoffs;
    private final Collection<Handoff> cleanupBuffer = new LinkedList<>();
    private final Map<IRI, ThingGiven> thingsGivenByActor;

    // a handoff must occur this many milliseconds after the "give" setup event
    private static final long GIVE_EXPIRATION = 20000;

    private final HandoffHandler handler;

    private final GestureLowPassFilter lowPassFilter;

    public HandoffMatcher(HandoffHandler handler) {
        this.handler = handler;
        this.latestHandoffs = new Stack<>();
        thingsGivenByActor = new HashMap<>();
        lowPassFilter = new GestureLowPassFilter(5000);
    }

    public void reset() {
        latestHandoffs.clear();
    }

    public synchronized void prepareForGive(final IRI actor, final IRI thingGivenIri, final long now) {
        ThingGiven tg = new ThingGiven();
        tg.thing = thingGivenIri;
        tg.timestamp = now;
        thingsGivenByActor.put(actor, tg);
    }

    private boolean isOld(final Handoff h,
                          final long now) {
        return now - h.timeOfSpike > SPIKE_MAX_GAP;
    }

    private void cleanup(final long timestamp) {
        cleanupBuffer.addAll(latestHandoffs.stream().filter(h -> isOld(h, timestamp)).collect(Collectors.toList()));

        if (cleanupBuffer.size() > 0) {
            cleanupBuffer.forEach(latestHandoffs::remove);

            cleanupBuffer.clear();
        }
    }

    public synchronized void receiveEvent(final IRI actor,
                                          final long timestamp) {
        //System.out.println("received handoff by " + actor + " at " + timestamp);
        cleanup(timestamp);

        Handoff gesture = new Handoff();
        gesture.actor = actor;
        gesture.timeOfSpike = timestamp;

        boolean matched = false;
        for (Handoff other : latestHandoffs) {
            System.out.println("checking handoff pair");
            if (other.matches(gesture)) {
                System.out.println("candidate handoff");
                // now we have a match, but one actor must also have "given" something prior to the gesture,
                // and not too long before the gesture.  Tie goes to the most recent "give"
                ThingGiven thisGiven = thingsGivenByActor.get(actor);
                ThingGiven otherGiven = thingsGivenByActor.get(other.actor);
                //System.out.println("thisGiven = " + thisGiven + ", otherGiven = " + otherGiven);
                if ((null != thisGiven && timestamp - thisGiven.timestamp <= GIVE_EXPIRATION)
                        || (null != otherGiven && timestamp - otherGiven.timestamp <= GIVE_EXPIRATION)) {
                    // consume both handoff half-gestures and produce a complete handoff event
                    matched(gesture);
                    matched(other);
                    matched = true;

                    // don't produce the gesture if it is redundant
                    if (lowPassFilter.doAllow(gesture.actor, other.actor, timestamp)) {
                        if (null != thisGiven && null != otherGiven) {
                            if (thisGiven.timestamp < otherGiven.timestamp) {
                                handler.handle(gesture, other, thisGiven.thing, timestamp);
                            } else {
                                handler.handle(other, gesture, otherGiven.thing, timestamp);
                            }
                        } else if (null != thisGiven) {
                            handler.handle(gesture, other, thisGiven.thing, timestamp);
                        } else {
                            handler.handle(other, gesture, otherGiven.thing, timestamp);
                        }
                    }
                    break;
                }
            }
        }

        if (!matched) {
            latestHandoffs.add(gesture);
            if (latestHandoffs.size() > STACK_SIZE_WARN_THRESHOLD) {
                long now = System.currentTimeMillis();
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

    private void matched(Handoff h) {
        h.matched = true;
    }

    public interface HandoffHandler {
        void handle(Handoff give, Handoff take, IRI thingGiven, long timestamp);
    }

    public class Handoff {
        public IRI actor;
        public long timeOfSpike;
        public boolean matched;

        public boolean matches(Handoff other) {
            return !other.actor.equals(actor)
                    && !matched && !other.matched
                    && Math.abs(timeOfSpike - other.timeOfSpike) <= SPIKE_MAX_GAP;
        }
    }

    public class ThingGiven {
        public IRI thing;
        public long timestamp;
    }
}
