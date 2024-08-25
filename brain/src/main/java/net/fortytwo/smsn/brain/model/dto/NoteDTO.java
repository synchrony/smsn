package net.fortytwo.smsn.brain.model.dto;

import net.fortytwo.smsn.SemanticSynchrony;
import net.fortytwo.smsn.brain.AtomId;
import net.fortytwo.smsn.brain.model.entities.ListNode;
import net.fortytwo.smsn.brain.model.entities.Note;
import net.fortytwo.smsn.brain.model.entities.Topic;

import java.util.Collection;
import java.util.HashMap;
import java.util.Map;
import java.util.function.Consumer;

public class NoteDTO implements Note {

    private final Map<String, Object> propertyMap;
    private Topic topic;
    private ListNode<Note> children;

    protected NoteDTO() {
        propertyMap = new HashMap<>();
    }

    public static NoteDTO createNew() {
        NoteDTO note = new NoteDTO();
        AtomId id = SemanticSynchrony.createRandomId();
        Note.setId(note, id);
        Note.setTitle(note, "note " + id);
        Note.setCreated(note, System.currentTimeMillis());
        Note.setWeight(note, SemanticSynchrony.DEFAULT_WEIGHT);
        Note.setPriority(note, SemanticSynchrony.DEFAULT_PRIORITY);
        // TODO: don't hard-code the default source
        Note.setSource(note, "public");
        return note;
    }

    @Override
    public void destroy() {
    }

    @Override
    public <V> V optProperty(String key) {
        return (V) propertyMap.get(key);
    }

    @Override
    public <V> V getProperty(String key) {
        return optProperty(key);
    }

    @Override
    public <V> void setProperty(String key, V value) {
        propertyMap.put(key, value);
    }

    @Override
    public Topic getTopic() {
        return topic;
    }

    @Override
    public void setTopic(final Topic topic) {
        this.topic = topic;
    }

    @Override
    public ListNode<Note> getChildren() {
        return children;
    }

    @Override
    public void setChildren(ListNode<Note> notes) {
        this.children = notes;
    }

    @Override
    public void forFirstOf(Consumer<ListNode<Note>> consumer) {
        throw new UnsupportedOperationException();
    }

    @Override
    public void addChildAt(Note child, int position) {
        throw new UnsupportedOperationException();
    }

    @Override
    public void deleteChildAt(int position) {
        throw new UnsupportedOperationException();
    }

    @Override
    public Collection<ListNode<Note>> getFirstOf() {
        throw new UnsupportedOperationException();
    }

    @Override
    public Note getSubject(ListNode<Note> notes) {
        throw new UnsupportedOperationException();
    }
}
