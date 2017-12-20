package net.fortytwo.smsn.brain.model.dto;

import net.fortytwo.smsn.brain.model.entities.Note;
import net.fortytwo.smsn.brain.model.entities.Topic;

import java.util.Collections;
import java.util.Iterator;
import java.util.function.Consumer;

public class TopicDTO implements Topic {
    private String id;

    @Override
    public void destroy() {
        // nothing to do
    }

    @Override
    public String getId() {
        return id;
    }

    @Override
    public void setId(final String id) {
        this.id = id;
    }

    @Override
    public boolean isIsolated() {
        return false;
    }

    @Override
    public Iterator<Note> getNotes() {
        return Collections.emptyIterator();
    }

    @Override
    public void forEachNote(Consumer<Note> visitor) {
        // nothing to do
    }
}
