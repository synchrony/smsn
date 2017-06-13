package net.fortytwo.smsn.brain.model.dto;

import net.fortytwo.smsn.brain.model.entities.EntityList;
import net.fortytwo.smsn.brain.model.entities.EntityTree;

public class TreeDTO<T> implements EntityTree<T> {

    private T value;
    private EntityList<EntityTree<T>> children;

    @Override
    public void destroy() {
        // nothing to do
    }

    @Override
    public T getValue() {
        return value;
    }

    @Override
    public void setValue(T value) {
        this.value = value;
    }

    @Override
    public EntityList<EntityTree<T>> getChildren() {
        return children;
    }

    @Override
    public void setChildren(EntityList<EntityTree<T>> children) {
        this.children = children;
    }

    static <T> void appendChild(EntityTree<T> tree, EntityTree<T> child) {
        EntityList<EntityTree<T>> toInsert = new ListDTO<>(child, null);

        EntityList<EntityTree<T>> cur = tree.getChildren();
        EntityList<EntityTree<T>> prev = null;
        while (null != cur) {
            prev = cur;
            cur = cur.getRest();
        }

        if (null == prev) {
            tree.setChildren(toInsert);
        } else {
            prev.setRest(toInsert);
        }
    }
}
