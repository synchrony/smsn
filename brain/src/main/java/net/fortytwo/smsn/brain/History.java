package net.fortytwo.smsn.brain;

import net.fortytwo.smsn.brain.model.entities.Note;
import net.fortytwo.smsn.brain.model.TopicGraph;
import net.fortytwo.smsn.brain.model.Filter;

import java.util.Collection;
import java.util.LinkedList;
import java.util.Optional;

public class History {
    private static final int CAPACITY = 1000;

    private final AtomId[] visited;
    private int totalVisits;

    public History() {
        this.visited = new AtomId[CAPACITY];
        totalVisits = 0;
    }

    public void visit(final AtomId noteId) {
        // repeated actions upon the same note count as a single visit
        if (totalVisits == 0 || !noteId.equals(getLastVisit())) {
            appendVisit(noteId);
        }
    }

    public Iterable<Note> getHistory(final int maxlen,
                                     final TopicGraph graph,
                                     final Filter filter) {
        Collection<Note> notes = new LinkedList<>();

        int low = Math.max(totalVisits - CAPACITY, 0);

        for (int i = totalVisits - 1; i >= low; i--) {
            if (notes.size() >= maxlen) {
                break;
            }

            AtomId id = visited[i % CAPACITY];

            Optional<Note> a = graph.getNoteById(id);
            if (a.isPresent() && filter.test(a.get())) {
                notes.add(a.get());
            }
        }

        return notes;
    }

    private AtomId getLastVisit() {
        return visited[(totalVisits - 1) % CAPACITY];
    }

    private void appendVisit(final AtomId noteId) {
        visited[totalVisits % CAPACITY] = noteId;
        totalVisits++;
    }
}
