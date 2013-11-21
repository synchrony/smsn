package net.fortytwo.extendo.brain;

import com.tinkerpop.blueprints.KeyIndexableGraph;
import com.tinkerpop.blueprints.impls.tg.TinkerGraph;
import net.fortytwo.extendo.Extendo;

import java.text.SimpleDateFormat;
import java.util.Date;
import java.util.HashMap;
import java.util.LinkedList;
import java.util.List;
import java.util.Map;

/**
 * @author Joshua Shinavier (http://fortytwo.net)
 */
public class EventStack {
    private static final SimpleDateFormat EVENT_TIME_FORMAT
            = new SimpleDateFormat("HH:mm:ss' on 'yyyy-MM-dd' 'Z");

    private final int capacity;

    private final Filter filter;
    private final BrainGraph graph;

    private final LinkedList<Atom> stack = new LinkedList<Atom>();

    private final RoutineNamer personNames = new RoutineNamer("person");

    public EventStack(final int capacity,
                      final Filter filter) {
        this.capacity = capacity;

        this.filter = filter;
        KeyIndexableGraph tg = new TinkerGraph();
        this.graph = new BrainGraph(tg);
    }

    public BrainGraph getInMemoryGraph() {
        return graph;
    }

    public List<Atom> getEvents() {
        return stack;
    }

    public void clear() {
        for (Atom event : stack) {
            deleteEvent(event);
        }
    }

    public void pushGestureEvent(final String expressedBy,
                                 final Date recognizedAt) {
        // TODO: use personal knowledge and Linked Data to find the person's name
        // Use this temporary name only if no actual name is discoverable
        String personName = personNames.getRoutineName(expressedBy);

        Atom gesture = createAtom();
        gesture.setValue(personName + " did something");

        // note: there will be duplicate people atoms in the in-memory graph
        Atom person = createAtom();
        person.setValue(personName);
        person.setAlias(expressedBy);

        Atom time = createAtom();
        time.setValue(EVENT_TIME_FORMAT.format(recognizedAt));

        gesture.setNotes(graph.createAtomList(person, time));

        push(gesture);
    }

    private void push(final Atom a) {
        while (stack.size() >= capacity) {
            Atom event = stack.removeLast();
            deleteEvent(event);
        }

        stack.push(a);
    }

    private void deleteEvent(final Atom event) {
        // TODO: in future, we will need to check for the presence of the stack item and its children in a registry before deletion, unless the registry atoms are created by copying
        graph.deleteListNodesAndChildrenRecursively(event.getNotes());
        graph.deleteAtom(event);
    }

    private Atom createAtom() {
        return graph.createAtom(filter, Extendo.createRandomKey());
    }

    private class RoutineNamer {
        private final String type;
        private final Map<String, Long> numberByName;
        private long count;

        public RoutineNamer(String type) {
            this.type = type;
            numberByName = new HashMap<String, Long>();
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
