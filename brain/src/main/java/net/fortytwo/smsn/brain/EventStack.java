package net.fortytwo.smsn.brain;

import hydra.util.Opt;
import net.fortytwo.smsn.SemanticSynchrony;

import java.text.SimpleDateFormat;
import java.util.ArrayList;
import java.util.Date;
import java.util.HashMap;
import java.util.LinkedList;
import java.util.List;
import java.util.Map;

public class EventStack {
    private static final SimpleDateFormat EVENT_TIME_FORMAT
            = new SimpleDateFormat("HH:mm:ss' on 'yyyy-MM-dd' 'Z");

    private final int capacity;

    private final LinkedList<TreeNode> stack = new LinkedList<>();

    private final RoutineNamer personNames = new RoutineNamer("person");

    public EventStack(final int capacity) {
        this.capacity = capacity;
    }

    public List<TreeNode> getEvents() {
        return stack;
    }

    public void clear() {
        stack.clear();
    }

    public TreeNode createGestureEvent(final String expressedBy,
                                   final Date recognizedAt) {
        // TODO: use personal knowledge and Linked Data to find the person's name
        // Use this temporary name only if no actual name is discoverable
        String personName = personNames.getRoutineName(expressedBy);

        // Create gesture event node
        long now = System.currentTimeMillis();

        // Create person node
        TreeNode person = createSimpleNode(
                personName,
                Opt.of(expressedBy) // alias
        );

        // Create time node
        TreeNode time = createSimpleNode(
                EVENT_TIME_FORMAT.format(recognizedAt),
                Opt.empty() // no alias
        );

        // Create gesture node with children
        List<TreeNode> children = new ArrayList<>();
        children.add(person);
        children.add(time);

        return new TreeNode(
                SemanticSynchrony.createRandomId(),
                new Timestamp(now),
                new Normed(SemanticSynchrony.DEFAULT_WEIGHT),
                Opt.empty(), // priority
                new SourceName("public"),
                personName + " did something",
                Opt.empty(), // alias
                Opt.empty(), // text
                Opt.empty(), // shortcut
                children,
                children.size(),
                0 // numberOfParents
        );
    }

    private TreeNode createSimpleNode(String title, Opt<String> alias) {
        long now = System.currentTimeMillis();

        return new TreeNode(
                SemanticSynchrony.createRandomId(),
                new Timestamp(now),
                new Normed(SemanticSynchrony.DEFAULT_WEIGHT),
                Opt.empty(), // priority
                new SourceName("public"),
                title,
                alias,
                Opt.empty(), // text
                Opt.empty(), // shortcut
                new ArrayList<>(), // no children
                0,
                0 // numberOfParents
        );
    }

    public void push(final TreeNode n) {
        TreeNode withIds = ensureIds(n);

        while (stack.size() >= capacity) {
            stack.removeLast();
        }

        stack.push(withIds);
    }

    // make the note look like it came from a graph (so it is compatible with Brain-mode views) by giving it an ID
    private TreeNode ensureIds(final TreeNode n) {
        AtomId id = n.id != null ? n.id : SemanticSynchrony.createRandomId();

        List<TreeNode> childrenWithIds = new ArrayList<>();
        for (TreeNode child : n.children) {
            childrenWithIds.add(ensureIds(child));
        }

        return new TreeNode(
                id,
                n.created,
                n.weight,
                n.priority,
                n.source,
                n.title,
                n.alias,
                n.text,
                n.shortcut,
                childrenWithIds,
                childrenWithIds.size(),
                n.numberOfParents
        );
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
