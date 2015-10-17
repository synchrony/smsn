package net.fortytwo.smsn.brain;

import com.tinkerpop.blueprints.Graph;
import com.tinkerpop.blueprints.Vertex;
import net.fortytwo.smsn.brain.wiki.NoteWriter;
import org.junit.After;
import org.junit.Before;
import org.junit.Test;

import java.util.Date;
import java.util.List;

import static org.junit.Assert.assertEquals;

/**
 * @author Joshua Shinavier (http://fortytwo.net)
 */
public class EventStackTest {
    private EventStack eventStack;

    private final int testCapacity = 50;

    private String agent1 = "http://example.org/people/arthurDent";
    private String agent2 = "http://example.org/people/triciaMcMillan";

    @Before
    public void setUp() throws Exception {
        eventStack = new EventStack(testCapacity);
    }

    @After
    public void tearDown() throws Exception {
        // the event stack is self-contained, occupies a small amount of memory,
        // and does not need to be specifically shut down
        eventStack.clear();
    }

    @Test
    public void testStack() throws Exception {
        List<Note> events = eventStack.getEvents();
        //BrainGraph bg = eventStack.getInMemoryGraph();
        //KeyIndexableGraph g = bg.getPropertyGraph();

        assertEquals(0, events.size());
        //assertEquals(0, countAtoms(bg));
        //assertEquals(0, countVertices(g));

        eventStack.push(eventStack.createGestureEvent(agent1, new Date()));
        assertEquals(1, events.size());
        //assertEquals(3, countAtoms(bg));
        //assertEquals(5, countVertices(g));

        Note e = events.get(0);
        assertEquals("person 1 did something", e.getValue());
        List<Note> kids = e.getChildren();
        assertEquals(2, kids.size());
        Note person = kids.get(0);
        Note time = kids.get(1);
        assertEquals("person 1", person.getValue());
        assertEquals(agent1, person.getAlias());
        assertEquals(2, time.getValue().indexOf(":"));

        eventStack.push(eventStack.createGestureEvent(agent2, new Date()));
        assertEquals(2, events.size());
        //assertEquals(6, countAtoms(bg));
        //assertEquals(10, countVertices(g));

        // verify that it is a stack, not a queue
        e = events.get(0);
        kids = e.getChildren();
        person = kids.get(0);
        assertEquals("person 2", person.getValue());
        assertEquals(agent2, person.getAlias());

        // push another event from agent #1 and verify that he is given the same routine name
        eventStack.push(eventStack.createGestureEvent(agent1, new Date()));
        assertEquals(3, events.size());
        //assertEquals(9, countAtoms(bg));
        //assertEquals(15, countVertices(g));
        e = events.get(0);
        kids = e.getChildren();
        person = kids.get(0);
        assertEquals("person 1", person.getValue());
        assertEquals(agent1, person.getAlias());

        // fill to capacity
        eventStack.clear();
        assertEquals(0, events.size());
        //assertEquals(0, countAtoms(bg));
        //assertEquals(0, countVertices(g));

        for (int i = 0; i < testCapacity; i++) {
            eventStack.push(eventStack.createGestureEvent(agent1, new Date()));
        }
        assertEquals(testCapacity, events.size());
        //assertEquals(3 * testCapacity, countAtoms(bg));
        //assertEquals(5 * testCapacity, countVertices(g));

        // accommodate further events, but drop events from the bottom of the stack
        eventStack.push(eventStack.createGestureEvent(agent2, new Date()));
        // the stack has not grown
        assertEquals(testCapacity, events.size());
        //assertEquals(3 * testCapacity, countAtoms(bg));
        //assertEquals(5 * testCapacity, countVertices(g));
        // this is the newest event
        assertEquals(agent2, eventStack.getEvents().get(0).getChildren().get(0).getAlias());

        // cleanup leaves nothing behind
        eventStack.clear();
        assertEquals(0, events.size());
        
        //assertEquals(0, countAtoms(bg));
        //assertEquals(0, countVertices(g));
    }

    @Test
    public void testViews() throws Exception {
        for (int i = 0; i < 3; i++) {
            eventStack.push(eventStack.createGestureEvent(0 == i % 2 ? agent1 : agent2, new Date()));
        }

        NoteWriter w = new NoteWriter();
        w.toWikiText(eventStack.getEvents(), System.out);
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
