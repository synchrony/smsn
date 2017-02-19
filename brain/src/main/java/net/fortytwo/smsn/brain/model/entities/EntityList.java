package net.fortytwo.smsn.brain.model.entities;

import java.util.List;

public interface EntityList<T extends Entity> extends Entity {

    String getId();

    T getFirst();

    boolean setFirst(T first);

    EntityList<T> getRest();

    boolean setRest(EntityList<T> rest);

    EntityList<T> getRestOf();

    List<T> toJavaList();
}
