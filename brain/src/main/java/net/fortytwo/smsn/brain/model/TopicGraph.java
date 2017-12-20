package net.fortytwo.smsn.brain.model;

import net.fortytwo.smsn.brain.model.entities.ListNode;
import net.fortytwo.smsn.brain.model.entities.Note;
import net.fortytwo.smsn.brain.model.entities.Topic;

import java.util.Optional;

/**
 * A graph of notes and lists conforming to the SmSn data model
 */
public interface TopicGraph {

    Optional<Topic> getTopicById(String id);

    String idOf(Topic topic);

    String iriOf(Topic topic);

    Topic createTopic(String id);

    Iterable<Note> getAllNotes();

    Iterable<Note> getNotesByAcronym(String acronym, Filter filter);

    Iterable<Note> getNotesByShortcut(String shortcut, Filter filter);

    Iterable<Note> getNotesByTitleQuery(String value, Filter filter);

    Note createNote(Topic topic, String label, Role role);

    Note createNoteWithProperties(Filter filter, Topic topic);

    void reindex(Note a);

    void begin();

    void commit();

    void rollback();

    TopicGraph createFilteredGraph(Filter filter);

    interface IORunnable {
        void run();
    }

    static void wrapInTransaction(final TopicGraph graph, final IORunnable runnable) {
        graph.begin();

        boolean success = false;
        try {
            runnable.run();
            success = true;
        } finally {
            if (success) {
                graph.commit();
            } else {
                graph.rollback();
            }
        }
    }
}
