package net.fortytwo.smsn.brain;

import net.fortytwo.smsn.SemanticSynchrony;
import net.fortytwo.smsn.brain.model.dto.NoteDTO;
import net.fortytwo.smsn.brain.model.entities.ListNode;
import net.fortytwo.smsn.brain.model.entities.Note;
import net.fortytwo.smsn.brain.query.Model;

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

    private final LinkedList<Note> stack = new LinkedList<>();

    private final RoutineNamer personNames = new RoutineNamer("person");

    public EventStack(final int capacity) {
        this.capacity = capacity;
    }

    public List<Note> getEvents() {
        return stack;
    }

    public void clear() {
        stack.clear();
    }

    public Note createGestureEvent(final String expressedBy,
                                   final Date recognizedAt) {
        // TODO: use personal knowledge and Linked Data to find the person's name
        // Use this temporary name only if no actual name is discoverable
        String personName = personNames.getRoutineName(expressedBy);

        Note gesture = new NoteDTO();
        gesture.setLabel(personName + " did something");

        // note: there will be duplicate people notes in the in-memory graph
        Note person = new NoteDTO();
        person.setLabel(personName);
        person.setAlias(expressedBy);

        Note time = new NoteDTO();
        time.setLabel(EVENT_TIME_FORMAT.format(recognizedAt));

        gesture.addChild(0, person);
        gesture.addChild(0, time);

        return gesture;
    }

    public void push(final Note note) {
        setIds(note);

        while (stack.size() >= capacity) {
            stack.removeLast();
        }

        stack.push(note);
    }

    // make the note look like it came from a graph (so it is compatible with Brain-mode views) by giving it an ID
    private void setIds(final Note note) {
        if (null == Model.getTopicId(note)) {
            Model.setTopicId(note, SemanticSynchrony.createRandomId());
        }

        ListNode.toJavaList(note.getFirst()).forEach(this::setIds);
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
            Long number = numberByName.computeIfAbsent(longName, k -> ++count);

            return type + " " + number;
        }
    }
}
