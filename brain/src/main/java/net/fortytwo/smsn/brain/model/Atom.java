package net.fortytwo.smsn.brain.model;

import java.util.Collection;
import java.util.function.Consumer;

public interface Atom {

    String getId();

    String getAlias();

    boolean setAlias(String alias);

    Long getCreated();

    boolean setCreated(Long created);

    // TODO: temporary
    String getValue();

    String getTitle();

    // TODO: temporary
    boolean setValue(String value);

    boolean setTitle(String title);

    String getPage();

    boolean setPage(String page);

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

    Collection<AtomList> getFirstOf();
}
