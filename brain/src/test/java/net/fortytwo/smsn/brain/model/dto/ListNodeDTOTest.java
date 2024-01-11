package net.fortytwo.smsn.brain.model.dto;

import net.fortytwo.smsn.brain.model.ListNodeTestBase;
import net.fortytwo.smsn.brain.model.entities.ListNode;
import net.fortytwo.smsn.brain.model.entities.Topic;

public class ListNodeDTOTest extends ListNodeTestBase {

    @Override
    protected ListNode<Topic> createListNode(final Topic... elements) {
        return ListNodeDTO.fromArray(elements);
    }
}
