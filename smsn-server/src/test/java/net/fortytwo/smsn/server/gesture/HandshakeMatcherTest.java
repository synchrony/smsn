package net.fortytwo.smsn.server.gesture;

import org.junit.Before;
import org.junit.Test;
import org.openrdf.model.IRI;
import org.openrdf.model.ValueFactory;
import org.openrdf.model.impl.SimpleValueFactory;

import java.util.Collections;
import java.util.HashSet;
import java.util.LinkedList;
import java.util.List;
import java.util.Set;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertTrue;

public class HandshakeMatcherTest {

    private static final ValueFactory valueFactory = SimpleValueFactory.getInstance();

    private final HandshakeMatcher server;

    private final Set<String> matches = new HashSet<>();
    private int totalMatches;

    // a typical handshake
    private long[] series1 = new long[]{0, 226, 417, 526, 612, 709};

    // another typical, but distinct handshake
    private long[] series2 = new long[]{0, 120, 191, 343, 405};

    private static final String BASE_IRI = "http://example.org/";

    private IRI
            actor1 = valueFactory.createIRI(BASE_IRI + "a1"),
            actor2 = valueFactory.createIRI(BASE_IRI + "a2");

    public HandshakeMatcherTest() {
        HandshakeMatcher.HandshakeHandler handler = new HandshakeMatcher.HandshakeHandler() {
            @Override
            public void handle(HandshakeMatcher.HandshakeSequence left, HandshakeMatcher.HandshakeSequence right, long timestamp) {
                totalMatches++;
                matches.add("" + left.actor + "," + right.actor);
                //System.out.println("handshake: " + left.actor + " and " + right.actor + " at " + timestamp);
            }
        };

        server = new HandshakeMatcher(handler);
    }

    @Before
    public void setUp() {
        totalMatches = 0;
        matches.clear();
        server.reset();
    }

    private void submitEvents(final IRI actor1,
                              final long[] series1,
                              final long offset1,
                              final IRI actor2,
                              final long[] series2,
                              final long offset2) {
        List<Event> events = new LinkedList<>();

        for (long l : series1) {
            Event e = new Event();
            e.actor = actor1;
            e.timestamp = l + offset1;
            events.add(e);
        }

        for (long l : series2) {
            Event e = new Event();
            e.actor = actor2;
            e.timestamp = l + offset2;
            events.add(e);
        }

        Collections.sort(events);

        /*System.out.println("events:");
        for (Event e : events) {
            System.out.println("\t" + e);
        }*/

        for (Event e : events) {
            long now = System.currentTimeMillis();
            server.receiveEvent(e.actor, e.timestamp, now);
        }
    }

    @Test
    public void testCompleteOverlap() throws Exception {
        submitEvents(actor1, series1, 0, actor2, series2, 0);
        // the handshakes match just once, even though there are several more events after they first match
        assertEquals(1, totalMatches);
        assertTrue(matches.contains(actor1.stringValue()+ "," + actor2.stringValue())
                || matches.contains(actor2.stringValue()+ "," + actor1.stringValue()));
    }

    @Test
    public void testNoOverlap() throws Exception {
        submitEvents(actor1, series1, 0, actor2, series2, 1000);
        assertEquals(0, totalMatches);
    }

    @Test
    public void testPartialOverlap() throws Exception {
        submitEvents(actor1, series1, 0, actor2, series2, 300);
        assertEquals(1, totalMatches);
    }

    private int handshakeCount;

    @Test
    public void testPerformance() throws Exception {
        int nShakes = 10000;
        int nPeaks = 5;
        long interShakeGap = 1000l;
        long interPeakGap = 160l;
        long offset = 10l;

        handshakeCount = 0;
        server.setHandler((left, right, time) -> handshakeCount++);

        long before = System.currentTimeMillis();
        long now = 0;
        for (int i = 0; i < nShakes; i++) {
            for (int j = 0; j < nPeaks; j++) {
                long actor1Peak = now;
                long actor2Peak = now + offset;
                server.receiveEvent(actor1, actor1Peak, actor1Peak);
                server.receiveEvent(actor2, actor2Peak, actor2Peak);
                now += interPeakGap;
            }
            now += interShakeGap;
        }
        long after = System.currentTimeMillis();
        System.out.println("matched " + handshakeCount + " handshakes in " + (after - before) + "ms");

        // TODO
        //assertEquals(nShakes, handshakeCount);
    }

    private class Event implements Comparable<Event> {
        public IRI actor;
        public long timestamp;

        @Override
        public int compareTo(Event other) {
            return ((Long) timestamp).compareTo(other.timestamp);
        }

        @Override
        public String toString() {
            return "(" + actor + "," + timestamp + ")";
        }
    }
}
