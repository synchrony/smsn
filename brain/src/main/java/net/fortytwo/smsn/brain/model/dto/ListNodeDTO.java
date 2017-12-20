package net.fortytwo.smsn.brain.model.dto;

import net.fortytwo.smsn.brain.model.entities.ListNode;

import java.util.function.BiFunction;

// TODO: make abstract
public class ListNodeDTO<T> implements ListNode<T> {
    private T first;
    private ListNode<T> rest;

    public ListNodeDTO() {
        this(null, null);
    }

    public ListNodeDTO(final T first, final ListNode<T> rest) {
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
    public void setFirst(T first) {
        this.first = first;
    }

    @Override
    public ListNode<T> getRest() {
        return rest;
    }

    @Override
    public void setRest(ListNode<T> rest) {
        this.rest = rest;
    }

    @Override
    public ListNode<T> getRestOf() {
        throw new UnsupportedOperationException();
    }

    @Override
    public int length() {
        return null == rest ? 1 : 1 + rest.length();
    }

    @Override
    public T get(int index) {
        return ListNode.get(this, index);
    }

    @Override
    public ListNode<T> add(int index, final T toAdd) {
        return ListNode.add(this, index, toAdd, (BiFunction<T, ListNode<T>, ListNode<T>>) ListNodeDTO::new);
    }

    @Override
    public ListNode<T> remove(int index) {
        return ListNode.remove(this, index);
    }

    public static <T> ListNode<T> fromArray(final T... elements) {
        ListNode<T> list = null;
        for (int i = elements.length - 1; i >= 0; i--) {
            list = new ListNodeDTO<>(elements[i], list);
        }
        return list;
    }
}
