package net.fortytwo.smsn.brain;

import org.junit.After;
import org.junit.Before;
import org.junit.Test;

import java.util.Date;
import java.util.List;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertTrue;

public class EventStackTest {
    private EventStack eventStack;

    private final int testCapacity = 50;

    private final String agent1 = "http://example.org/people/arthurDent";
    private final String agent2 = "http://example.org/people/triciaMcMillan";

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
        List<TreeNode> events = eventStack.getEvents();

        assertEquals(0, events.size());

        eventStack.push(eventStack.createGestureEvent(agent1, new Date()));
        assertEquals(1, events.size());

        TreeNode e = events.get(0);
        assertEquals("person 1 did something", e.title);
        List<TreeNode> kids = e.children;
        assertEquals(2, kids.size());
        TreeNode person = kids.get(0);
        TreeNode time = kids.get(1);
        assertEquals("person 1", person.title);
        assertTrue(person.alias.isPresent());
        assertEquals(agent1, person.alias.get());
        assertEquals(2, time.title.indexOf(":"));

        eventStack.push(eventStack.createGestureEvent(agent2, new Date()));
        assertEquals(2, events.size());

        // verify that it is a stack, not a queue
        e = events.get(0);
        kids = e.children;
        person = kids.get(0);
        assertEquals("person 2", person.title);
        assertTrue(person.alias.isPresent());
        assertEquals(agent2, person.alias.get());

        // push another event from agent #1 and verify that he is given the same routine name
        eventStack.push(eventStack.createGestureEvent(agent1, new Date()));
        assertEquals(3, events.size());
        e = events.get(0);
        kids = e.children;
        person = kids.get(0);
        assertEquals("person 1", person.title);
        assertTrue(person.alias.isPresent());
        assertEquals(agent1, person.alias.get());

        // fill to capacity
        eventStack.clear();
        assertEquals(0, events.size());

        for (int i = 0; i < testCapacity; i++) {
            eventStack.push(eventStack.createGestureEvent(agent1, new Date()));
        }
        assertEquals(testCapacity, events.size());

        // accommodate further events, but drop events from the bottom of the stack
        eventStack.push(eventStack.createGestureEvent(agent2, new Date()));
        // the stack has not grown
        assertEquals(testCapacity, events.size());
        // this is the newest event
        TreeNode firstEvent = eventStack.getEvents().get(0);
        TreeNode firstChild = firstEvent.children.get(0);
        assertTrue(firstChild.alias.isPresent());
        assertEquals(agent2, firstChild.alias.get());

        // cleanup leaves nothing behind
        eventStack.clear();
        assertEquals(0, events.size());
    }

    @Test
    public void testViews() throws Exception {
        for (int i = 0; i < 3; i++) {
            eventStack.push(eventStack.createGestureEvent(0 == i % 2 ? agent1 : agent2, new Date()));
        }

        // Just verify we can iterate over events
        for (TreeNode event : eventStack.getEvents()) {
            assertTrue(event.title.contains("did something"));
            assertEquals(2, event.children.size());
        }
    }
}
