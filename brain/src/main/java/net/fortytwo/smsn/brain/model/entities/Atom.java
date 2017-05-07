package net.fortytwo.smsn.brain.model.entities;

import java.util.Collection;
import java.util.function.Consumer;

public interface Atom extends Entity {

    String getId();

    void setId(String id);

    String getAlias();

    void setAlias(String alias);

    Long getCreated();

    void setCreated(Long created);

    String getTitle();

    void setTitle(String title);

    String getText();

    void setText(String text);

    Float getPriority();

    void setPriority(Float priority);

    Float getSharability();

    void setSharability(Float sharability);

    String getShortcut();

    void setShortcut(String shortcut);

    Float getWeight();

    void setWeight(Float weight);

    EntityList<Atom> getChildren();

    void setChildren(EntityList<Atom> notes);

    void forFirstOf(Consumer<EntityList<Atom>> consumer);

    void addChildAt(final Atom child, int position);

    void deleteChildAt(int position);

    Collection<EntityList<Atom>> getFirstOf();

    Atom getSubject(EntityList<Atom> notes);

    String getSource();

    void setSource(String source);
}
