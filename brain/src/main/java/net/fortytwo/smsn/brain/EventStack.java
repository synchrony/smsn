package net.fortytwo.smsn.brain;

import net.fortytwo.smsn.SemanticSynchrony;
import net.fortytwo.smsn.brain.model.dto.TreeNodeDTO;
import net.fortytwo.smsn.brain.model.entities.Link;
import net.fortytwo.smsn.brain.model.entities.TreeNode;
import net.fortytwo.smsn.brain.query.TreeViews;

import java.text.SimpleDateFormat;
import java.util.Date;
import java.util.HashMap;
import java.util.LinkedList;
import java.util.List;
import java.util.Map;

public class EventStack {
    private static final SimpleDateFormat EVENT_TIME_FORMAT
            = new SimpleDateFormat("HH:mm:ss' on 'yyyy-MM-dd' 'Z");

    private final int capacity;

    private final LinkedList<TreeNode<Link>> stack = new LinkedList<>();

    private final RoutineNamer personNames = new RoutineNamer("person");

    public EventStack(final int capacity) {
        this.capacity = capacity;
    }

    public List<TreeNode<Link>> getEvents() {
        return stack;
    }

    public void clear() {
        stack.clear();
        //for (Atom event : stack) {
        //    deleteEvent(event);
        //}
    }

    public TreeNode<Link> createGestureEvent(final String expressedBy,
                                   final Date recognizedAt) {
        // TODO: use personal knowledge and Linked Data to find the person's name
        // Use this temporary name only if no actual name is discoverable
        String personName = personNames.getRoutineName(expressedBy);

        //Atom gesture = createAtom();
        TreeNode<Link> gesture = TreeNodeDTO.createEmptyNode();
        TreeViews.setTitle(gesture, personName + " did something");

        // note: there will be duplicate people atoms in the in-memory graph
        TreeNode<Link> person = TreeNodeDTO.createEmptyNode();
        TreeViews.setTitle(person, personName);
        TreeViews.setAlias(person, expressedBy);

        TreeNode<Link> time = TreeNodeDTO.createEmptyNode();
        TreeViews.setTitle(time, EVENT_TIME_FORMAT.format(recognizedAt));

        gesture.addChild(person);
        gesture.addChild(time);

        return gesture;
    }

    public void push(final TreeNode<Link> n) {
        setIds(n);

        while (stack.size() >= capacity) {
            stack.removeLast();
            //Atom event = stack.removeLast();
            //deleteEvent(event);
        }

        stack.push(n);
    }

    // make the note look like it came from a graph (so it is compatible with Brain-mode views) by giving it an ID
    private void setIds(final TreeNode<Link> n) {
        if (null == TreeViews.getId(n)) {
            TreeViews.setId(n, SemanticSynchrony.createRandomId());
        }

        TreeViews.getChildrenAsList(n).forEach(this::setIds);
    }

    // note: instances of this class currently grow without bound
    private class RoutineNamer {
        private final String type;
        private final Map<String, Long> numberByName;
        private long count;

        public RoutineNamer(final String type) {
            this.type = type;
            numberByName = new HashMap<>();
        }

        public String getRoutineName(final String longName) {
            Long number = numberByName.get(longName);
            if (null == number) {
                number = ++count;
                numberByName.put(longName, number);
            }

            return type + " " + number;
        }
    }
}
