package net.fortytwo.smsn.brain.model.dto;

import net.fortytwo.smsn.SemanticSynchrony;
import net.fortytwo.smsn.brain.model.entities.ListNode;
import net.fortytwo.smsn.brain.model.entities.Note;
import net.fortytwo.smsn.brain.model.entities.Topic;

import java.util.HashMap;
import java.util.Map;

public class NoteDTO extends ListNodeDTO<Note> implements Note {

    private final Map<String, Object> propertyMap;
    private Topic topic;

    private int numberOfChildren;
    private int numberOfParents;

    public NoteDTO() {
        super();

        propertyMap = new HashMap<>();

        setLabel("default note");
        setCreated(System.currentTimeMillis());
        setWeight(SemanticSynchrony.DEFAULT_WEIGHT);
        setPriority(SemanticSynchrony.DEFAULT_PRIORITY);
        // TODO: don't hard-code the default source
        setSource("public");
    }

    @Override
    public void destroy() {
    }

    @Override
    public <V> V getProperty(String key) {
        return (V) propertyMap.get(key);
    }

    @Override
    public <V> void setProperty(String key, V value) {
        propertyMap.put(key, value);
    }

    @Override
    public int getNumberOfChildren() {
        return numberOfChildren;
    }

    @Override
    public int getNumberOfParents() {
        return numberOfParents;
    }

    public void setNumberOfChildren(int number) {
        this.numberOfChildren = number;
    }

    public void setNumberOfParents(int number) {
        this.numberOfParents = number;
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
    public Note getParent() {
        return null;
    }

    @Override
    public void addChild(int index, Note child) {
        ListNode.add(this, index, child, (note, rest) -> {
            setRest(rest);
            return note;
        });
    }

    @Override
    public void removeChild(int index) {
        ListNode.remove(this, index);
    }
}
