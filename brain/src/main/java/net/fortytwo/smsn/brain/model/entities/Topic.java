package net.fortytwo.smsn.brain.model.entities;

import java.util.Iterator;
import java.util.function.Consumer;

public interface Topic extends Entity {

    String getId();

    void setId(String id);

    boolean isIsolated();

    Iterator<Note> getNotes();

    void forEachNote(Consumer<Note> visitor);
}
