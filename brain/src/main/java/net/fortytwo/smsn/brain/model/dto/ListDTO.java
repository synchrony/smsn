package net.fortytwo.smsn.brain.model.dto;

import net.fortytwo.smsn.brain.model.entities.EntityList;

import java.util.LinkedList;
import java.util.List;

public class ListDTO<T> implements EntityList<T> {
    private T first;
    private EntityList<T> rest;

    public ListDTO(final T first, final EntityList<T> rest) {
        this.first = first;
        this.rest = rest;
    }

    @Override
    public void destroy() {
        // nothing to do
    }

    @Override
    public T getFirst() {
        return first;
    }

    @Override
    public boolean setFirst(T first) {
        this.first = first;
        return false;
    }

    @Override
    public EntityList<T> getRest() {
        return rest;
    }

    @Override
    public boolean setRest(EntityList<T> rest) {
        this.rest = rest;
        return false;
    }

    @Override
    public EntityList<T> getRestOf() {
        throw new UnsupportedOperationException();
    }
}
