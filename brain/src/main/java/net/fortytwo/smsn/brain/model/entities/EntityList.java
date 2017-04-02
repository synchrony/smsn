package net.fortytwo.smsn.brain.model.entities;

import java.util.LinkedList;
import java.util.List;

public interface EntityList<T> extends Entity {

    T getFirst();

    boolean setFirst(T first);

    EntityList<T> getRest();

    boolean setRest(EntityList<T> rest);

    EntityList<T> getRestOf();

    static <T> List<T> toJavaList(EntityList<T> list) {
        List<T> javaList = new LinkedList<T>();
        EntityList<T> cur = list;
        while (null != cur) {
            javaList.add(cur.getFirst());
            cur = cur.getRest();
        }
        return javaList;
    }
}
