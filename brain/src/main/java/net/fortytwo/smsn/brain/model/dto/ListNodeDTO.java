package net.fortytwo.smsn.brain.model.dto;

import net.fortytwo.smsn.brain.model.entities.ListNode;

public class ListNodeDTO<T> implements ListNode<T> {
    private T first;
    private ListNode<T> rest;

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
    public boolean setFirst(T first) {
        this.first = first;
        return false;
    }

    @Override
    public ListNode<T> getRest() {
        return rest;
    }

    @Override
    public boolean setRest(ListNode<T> rest) {
        this.rest = rest;
        return false;
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
        return 0 == index ? first : rest.get(index - 1);
    }

    public static <T> ListNode<T> fromArray(final T... elements) {
        ListNode<T> list = null;
        for (int i = elements.length - 1; i >= 0; i--) {
            list = new ListNodeDTO<>(elements[i], list);
        }
        return list;
    }
}
