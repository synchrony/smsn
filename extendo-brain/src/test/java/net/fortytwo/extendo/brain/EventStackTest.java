package net.fortytwo.extendo.brain;

import com.tinkerpop.blueprints.Graph;
import com.tinkerpop.blueprints.KeyIndexableGraph;
import com.tinkerpop.blueprints.Vertex;
import net.fortytwo.extendo.brain.wiki.NoteWriter;
import org.junit.After;
import org.junit.Before;
import org.junit.Test;

import java.util.Date;
import java.util.List;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertTrue;

/**
 * @author Joshua Shinavier (http://fortytwo.net)
 */
public class EventStackTest {
    private EventStack eventStack;
    private Filter filter;

    private final int testCapacity = 50;

    private String agent1 = "http://example.org/people/arthurDent";
    private String agent2 = "http://example.org/people/triciaMcMillan";

    @Before
    public void setUp() throws Exception {
        filter = new Filter(0f, 1f, 0.5f, 0f, 1f, 0.5f);

        eventStack = new EventStack(testCapacity, filter);
    }

    @After
    public void tearDown() throws Exception {
        // the event stack is self-contained, occupies a small amount of memory,
        // and does not need to be specifically shut down
        eventStack.clear();
    }

    @Test
    public void testStack() throws Exception {
        BrainGraph bg = eventStack.getInMemoryGraph();
        KeyIndexableGraph g = bg.getGraph();

        assertEquals(0, countAtoms(bg));
        assertEquals(0, countVertices(g));

        eventStack.pushGestureEvent(agent1, new Date());
        assertEquals(3, countAtoms(bg));
        assertEquals(5, countVertices(g));

        List<Atom> events = eventStack.getEvents();
        assertEquals(1, events.size());
        Atom e = events.get(0);
        assertEquals("person 1 did something", e.getValue());
        AtomList kids = e.getNotes();
        Atom person = kids.getFirst();
        Atom time = kids.getRest().getFirst();
        assertTrue(null == kids.getRest().getRest());
        assertEquals("person 1", person.getValue());
        assertEquals(agent1, person.getAlias());
        assertEquals(2, time.getValue().indexOf(":"));

        eventStack.pushGestureEvent(agent2, new Date());
        assertEquals(6, countAtoms(bg));
        assertEquals(10, countVertices(g));

        // verify that it is a stack, not a queue
        assertEquals(2, events.size());
        e = events.get(0);
        kids = e.getNotes();
        person = kids.getFirst();
        assertEquals("person 2", person.getValue());
        assertEquals(agent2, person.getAlias());

        // push another event from agent #1 and verify that he is given the same routine name
        eventStack.pushGestureEvent(agent1, new Date());
        assertEquals(9, countAtoms(bg));
        assertEquals(15, countVertices(g));
        assertEquals(3, events.size());
        e = events.get(0);
        kids = e.getNotes();
        person = kids.getFirst();
        assertEquals("person 1", person.getValue());
        assertEquals(agent1, person.getAlias());

        // fill to capacity
        eventStack.clear();
        assertEquals(0, countAtoms(bg));
        assertEquals(0, countVertices(g));

        for (int i = 0; i < testCapacity; i++) {
            eventStack.pushGestureEvent(agent1, new Date());
        }
        assertEquals(3 * testCapacity, countAtoms(bg));
        assertEquals(5 * testCapacity, countVertices(g));

        // accommodate further events, but drop events from the bottom of the stack
        eventStack.pushGestureEvent(agent2, new Date());
        // the stack has not grown
        assertEquals(3 * testCapacity, countAtoms(bg));
        assertEquals(5 * testCapacity, countVertices(g));
        // this is the newest event
        assertEquals(agent2, eventStack.getEvents().get(0).getNotes().getFirst().getAlias());

        // cleanup leaves nothing behind
        eventStack.clear();
        assertEquals(0, countAtoms(bg));
        assertEquals(0, countVertices(g));
    }

    @Test
    public void testViews() throws Exception {
        NoteQueries q = new NoteQueries(eventStack.getInMemoryGraph());

        for (int i = 0; i < 3; i++) {
            eventStack.pushGestureEvent(0 == i % 2 ? agent1 : agent2, new Date());
        }

        Note n = q.view(eventStack.getEvents(), 2, filter);

        NoteWriter w = new NoteWriter();
        w.writeNotes(n.getChildren(), System.out);
    }

    private long countAtoms(final BrainGraph bg) {
        long count = 0;
        for (Atom a : bg.getAtoms()) {
            count++;
        }

        return count;
    }

    private long countVertices(final Graph g) {
        long count = 0;
        for (Vertex v : g.getVertices()) {
            count++;
        }

        return count;
    }
}
