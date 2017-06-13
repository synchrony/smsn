package net.fortytwo.smsn.brain.model.entities;

public interface EntityTree<T> extends Entity {

    T getValue();

    void setValue(T value);

    EntityList<EntityTree<T>> getChildren();

    void setChildren(EntityList<EntityTree<T>> children);
}
