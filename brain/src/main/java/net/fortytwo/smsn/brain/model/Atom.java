package net.fortytwo.smsn.brain.model;

import java.util.function.Consumer;

/**
 * @author Joshua Shinavier (http://fortytwo.net)
 */
public interface Atom extends GraphEntity {

    String getAlias();

    boolean setAlias(String alias);

    Long getCreated();

    boolean setCreated(Long created);

    String getValue();

    boolean setValue(String value);

    Float getPriority();

    boolean setPriority(Float priority);

    Float getSharability();

    boolean setSharability(Float sharability);

    String getShortcut();

    boolean setShortcut(String shortcut);

    Float getWeight();

    boolean setWeight(Float weight);

    AtomList getNotes();

    boolean setNotes(AtomList notes);

    void forFirstOf(Consumer<AtomList> consumer);

    void addChildAt(final Atom child, int position);

    void deleteChildAt(int position);
}
