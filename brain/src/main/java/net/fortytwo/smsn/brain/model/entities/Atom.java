package net.fortytwo.smsn.brain.model.entities;

import java.util.Collection;
import java.util.function.Consumer;

public interface Atom extends Entity {

    String getId();

    boolean setId(String id);

    String getAlias();

    boolean setAlias(String alias);

    Long getCreated();

    boolean setCreated(Long created);

    String getTitle();

    boolean setTitle(String title);

    String getText();

    boolean setText(String text);

    Float getPriority();

    boolean setPriority(Float priority);

    Float getSharability();

    boolean setSharability(Float sharability);

    String getShortcut();

    boolean setShortcut(String shortcut);

    Float getWeight();

    boolean setWeight(Float weight);

    EntityList<Atom> getNotes();

    boolean setNotes(EntityList<Atom> notes);

    void forFirstOf(Consumer<EntityList<Atom>> consumer);

    void addChildAt(final Atom child, int position);

    void deleteChildAt(int position);

    Collection<EntityList<Atom>> getFirstOf();

    // TODO: temporary solution
    Atom getSubject(EntityList<Atom> notes);
}
