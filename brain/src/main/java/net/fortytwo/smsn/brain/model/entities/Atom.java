package net.fortytwo.smsn.brain.model.entities;

import java.util.Collection;
import java.util.function.Consumer;

public interface Atom extends Node {

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

    String getShortcut();

    void setShortcut(String shortcut);

    Float getWeight();

    void setWeight(Float weight);

    ListNode<Atom> getChildren();

    void setChildren(ListNode<Atom> notes);

    void forFirstOf(Consumer<ListNode<Atom>> consumer);

    void addChildAt(final Atom child, int position);

    void deleteChildAt(int position);

    Collection<ListNode<Atom>> getFirstOf();

    Atom getSubject(ListNode<Atom> notes);

    String getSource();

    void setSource(String source);
}
