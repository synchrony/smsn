package net.fortytwo.smsn.brain;

import net.fortytwo.smsn.brain.io.wiki.WikiPrinter;
import net.fortytwo.smsn.brain.model.dto.PageDTO;
import net.fortytwo.smsn.brain.model.entities.Link;
import net.fortytwo.smsn.brain.model.entities.Page;
import net.fortytwo.smsn.brain.model.entities.TreeNode;
import net.fortytwo.smsn.brain.query.TreeViews;
import org.junit.After;
import org.junit.Before;
import org.junit.Test;

import java.util.Date;
import java.util.List;

import static org.junit.Assert.assertEquals;

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
        List<TreeNode<Link>> events = eventStack.getEvents();
        //BrainGraph bg = eventStack.getInMemoryGraph();
        //KeyIndexableGraph g = bg.getPropertyGraph();

        assertEquals(0, events.size());
        //assertEquals(0, countAtoms(bg));
        //assertEquals(0, countVertices(g));

        eventStack.push(eventStack.createGestureEvent(agent1, new Date()));
        assertEquals(1, events.size());
        //assertEquals(3, countAtoms(bg));
        //assertEquals(5, countVertices(g));

        TreeNode<Link> e = events.get(0);
        assertEquals("person 1 did something", TreeViews.getTitle(e));
        List<TreeNode<Link>> kids = TreeViews.getChildrenAsList(e);
        assertEquals(2, kids.size());
        TreeNode<Link> person = kids.get(0);
        TreeNode<Link> time = kids.get(1);
        assertEquals("person 1", TreeViews.getTitle(person));
        assertEquals(agent1, TreeViews.getAlias(person));
        assertEquals(2, TreeViews.getTitle(time).indexOf(":"));

        eventStack.push(eventStack.createGestureEvent(agent2, new Date()));
        assertEquals(2, events.size());
        //assertEquals(6, countAtoms(bg));
        //assertEquals(10, countVertices(g));

        // verify that it is a stack, not a queue
        e = events.get(0);
        kids = TreeViews.getChildrenAsList(e);
        person = kids.get(0);
        assertEquals("person 2", TreeViews.getTitle(person));
        assertEquals(agent2, TreeViews.getAlias(person));

        // push another event from agent #1 and verify that he is given the same routine name
        eventStack.push(eventStack.createGestureEvent(agent1, new Date()));
        assertEquals(3, events.size());
        //assertEquals(9, countAtoms(bg));
        //assertEquals(15, countVertices(g));
        e = events.get(0);
        kids = TreeViews.getChildrenAsList(e);
        person = kids.get(0);
        assertEquals("person 1", TreeViews.getTitle(person));
        assertEquals(agent1, TreeViews.getAlias(person));

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
        assertEquals(agent2, TreeViews.getAlias(eventStack.getEvents().get(0).getChildren().get(0)));

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

        WikiPrinter w = new WikiPrinter(System.out);
        for (TreeNode<Link> event : eventStack.getEvents()) {
            Page page = PageDTO.createTransitional();
            page.setContent(event);
            w.print(page);
        }
    }
}
