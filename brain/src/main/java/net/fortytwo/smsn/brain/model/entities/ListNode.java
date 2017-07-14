package net.fortytwo.smsn.brain.model.entities;

import java.util.LinkedList;
import java.util.List;
import java.util.function.BiFunction;

public interface ListNode<T> extends Node {

    T getFirst();

    void setFirst(T first);

    ListNode<T> getRest();

    void setRest(ListNode<T> rest);

    ListNode<T> getRestOf();

    int length();

    T get(int index);

    ListNode<T> add(int index, T toAdd);

    ListNode<T> remove(int index);

    static <T> List<T> toJavaList(ListNode<T> list) {
        List<T> javaList = new LinkedList<>();
        ListNode<T> cur = list;
        while (null != cur) {
            javaList.add(cur.getFirst());
            cur = cur.getRest();
        }
        return javaList;
    }

    static <T> T get(final ListNode<T> node, int index) {
        if (0 > index) {
            indexOutOfBounds();
        } else if (0 == index) {
            return node.getFirst();
        } else if (null == node.getRest()) {
            indexOutOfBounds();
        } else {
            return node.getRest().get(index - 1);
        }

        return null;
    }

    static <T> ListNode<T> add(final ListNode<T> node, int index, final T toAdd,
                               final BiFunction<T, ListNode<T>, ListNode<T>> constructor) {
        if (0 > index) {
            indexOutOfBounds();
        }

        if (0 == index) {
            return constructor.apply(toAdd, node);
        } else {
            if (null == node.getRest()) {
                if (1 == index) {
                    node.setRest(constructor.apply(toAdd, null));
                } else {
                    indexOutOfBounds();
                }
            } else {
                node.getRest().add(index - 1, toAdd);
            }

            return node;
        }
    }

    static <T> ListNode<T> remove(final ListNode<T> node, int index) {
        if (0 > index) {
            indexOutOfBounds();
        }

        if (0 == index) {
            return node.getRest();
        } else {
            ListNode<T> cur = node, prev = null;
            while (0 < index) {
                prev = cur;
                cur = cur.getRest();
                index--;
            }

            if (null == cur) {
                indexOutOfBounds();
            }
            prev.setRest(cur.getRest());

            return node;
        }
    }

    static void indexOutOfBounds() {
        throw new IllegalArgumentException("index out of bounds");
    }
}
