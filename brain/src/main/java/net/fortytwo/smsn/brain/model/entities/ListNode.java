package net.fortytwo.smsn.brain.model.entities;

import java.util.LinkedList;
import java.util.List;

public interface ListNode<T> extends Node {

    T getFirst();

    boolean setFirst(T first);

    ListNode<T> getRest();

    boolean setRest(ListNode<T> rest);

    ListNode<T> getRestOf();

    int length();

    T get(int index);

    static <T> List<T> toJavaList(ListNode<T> list) {
        List<T> javaList = new LinkedList<>();
        ListNode<T> cur = list;
        while (null != cur) {
            javaList.add(cur.getFirst());
            cur = cur.getRest();
        }
        return javaList;
    }
}
