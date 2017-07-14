package net.fortytwo.smsn.brain.model.pg;

import net.fortytwo.smsn.brain.model.ListNodeTestBase;
import net.fortytwo.smsn.brain.model.entities.ListNode;
import net.fortytwo.smsn.brain.model.entities.Topic;

public class PGListNodeTest extends ListNodeTestBase {

    @Override
    protected ListNode<Topic> createListNode(final Topic... elements) {
        return topicGraph.createListOfTopics(elements);
    }
}
