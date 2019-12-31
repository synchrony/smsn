package net.fortytwo.smsn.brain.model.dto;

import net.fortytwo.smsn.SemanticSynchrony;
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

    public NoteDTO() {
        propertyMap = new HashMap<>();

        String id = SemanticSynchrony.createRandomId();
        Note.setId(this, id);
        Note.setTitle(this, "note " + id);
        Note.setCreated(this, System.currentTimeMillis());
        Note.setWeight(this, SemanticSynchrony.DEFAULT_WEIGHT);
        Note.setPriority(this, SemanticSynchrony.DEFAULT_PRIORITY);
        // TODO: don't hard-code the default source
        Note.setSource(this, "public");
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
