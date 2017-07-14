package net.fortytwo.smsn.brain.model;

import net.fortytwo.smsn.brain.BrainTestBase;
import net.fortytwo.smsn.brain.model.entities.ListNode;
import net.fortytwo.smsn.brain.model.entities.Topic;
import org.junit.Test;

import static org.junit.Assert.assertEquals;

public abstract class ListNodeTestBase extends BrainTestBase {

    protected abstract ListNode<Topic> createListNode(Topic... elements);

    @Test
    public void simpleConstructionSucceeds() {
        ListNode<Topic> list = createListNode(arthurTopic, fordTopic);
        assertEquals(2, list.length());
        assertEquals(arthurTopic, list.get(0));
        assertEquals(fordTopic, list.get(1));
    }

    @Test(expected = IllegalArgumentException.class)
    public void getAtNegativeIndexFails() {
        ListNode<Topic> node = createListNode(arthurTopic);
        node.get(-1);
    }

    @Test(expected = IllegalArgumentException.class)
    public void getAtTooHighIndexFails() {
        ListNode<Topic> node = createListNode(arthurTopic);
        node.get(1);
    }

    @Test(expected = IllegalArgumentException.class)
    public void addAtTooLowIndexFails() {
        ListNode<Topic> node = createListNode(arthurTopic);
        node.add(-1, fordTopic);
    }

    @Test(expected = IllegalArgumentException.class)
    public void addAtTooHighIndexFails() {
        ListNode<Topic> node = createListNode(arthurTopic);
        node.add(2, fordTopic);
    }

    @Test
    public void addAtZeroIndexSucceeds() {
        ListNode<Topic> node = createListNode(arthurTopic);
        ListNode<Topic> modified = node.add(0, fordTopic);
        assertEquals(2, modified.length());
        assertEquals(fordTopic, modified.get(0));
        assertEquals(arthurTopic, modified.get(1));
    }

    @Test
    public void addAtLengthIndexSucceeds() {
        ListNode<Topic> node = createListNode(arthurTopic);
        ListNode<Topic> modified = node.add(1, fordTopic);
        assertEquals(2, modified.length());
        assertEquals(arthurTopic, modified.get(0));
        assertEquals(fordTopic, modified.get(1));
    }

    @Test(expected = IllegalArgumentException.class)
    public void removeFromTooLowIndexFails() {
        ListNode<Topic> node = createListNode(arthurTopic);
        node.remove(-1);
    }

    @Test(expected = IllegalArgumentException.class)
    public void removeFromTooHighIndexFails() {
        ListNode<Topic> node = createListNode(arthurTopic);
        node.remove(1);
    }

    @Test
    public void removeFromZeroIndexSucceeds() {
        ListNode<Topic> node = createListNode(arthurTopic, fordTopic);
        ListNode<Topic> modified = node.remove(0);
        assertEquals(1, modified.length());
        assertEquals(fordTopic, modified.get(0));
    }

    @Test
    public void removeFromLastIndexSucceeds() {
        ListNode<Topic> node = createListNode(arthurTopic, fordTopic);
        ListNode<Topic> modified = node.remove(1);
        assertEquals(1, modified.length());
        assertEquals(arthurTopic, modified.get(0));
    }
}
