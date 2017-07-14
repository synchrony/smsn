package net.fortytwo.smsn.brain.model.entities;

import net.fortytwo.smsn.SemanticSynchrony;
import net.fortytwo.smsn.brain.model.Property;

import java.util.Collection;
import java.util.LinkedHashMap;
import java.util.Map;
import java.util.function.Consumer;

public interface Atom extends Node {

    Map<String, Property<Atom, ?>> propertiesByKey = createPropertiesByKey();

    static Map<String, Property<Atom, ?>> createPropertiesByKey() {
        Map<String, Property<Atom, ?>> propertiesByKey = new LinkedHashMap<>();
        for (Property<Atom, ?> prop : new Property[]{
                new Property<>(true, true, SemanticSynchrony.PropertyKeys.ALIAS,
                        Atom::getAlias, Atom::setAlias, s -> s),
                new Property<>(true, true, SemanticSynchrony.PropertyKeys.CREATED,
                        Atom::getCreated, Atom::setCreated, Long::valueOf),
                new Property<>(true, false, SemanticSynchrony.PropertyKeys.TEXT,
                        Atom::getText, Atom::setText, s -> s),
                new Property<>(true, true, SemanticSynchrony.PropertyKeys.SOURCE,
                        Atom::getSource, Atom::setSource, s -> s),
                new Property<>(true, true, SemanticSynchrony.PropertyKeys.SHORTCUT,
                        Atom::getShortcut, Atom::setShortcut, s -> s),
                new Property<>(true, true, SemanticSynchrony.PropertyKeys.TITLE,
                        Atom::getTitle, Atom::setTitle, s -> s),
                new Property<>(true, true, SemanticSynchrony.PropertyKeys.WEIGHT,
                        Atom::getWeight, Atom::setWeight, Float::valueOf),
                new Property<>(true, true, SemanticSynchrony.PropertyKeys.PRIORITY,
                        Atom::getPriority, Atom::setPriority, Float::valueOf),
        }) {
            propertiesByKey.put(prop.getPropertyKey(), prop);
        }
        return propertiesByKey;
    }

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

    String getSource();

    void setSource(String source);

    ListNode<Atom> getChildren();

    void setChildren(ListNode<Atom> notes);

    void forFirstOf(Consumer<ListNode<Atom>> consumer);

    void addChildAt(final Atom child, int position);

    void deleteChildAt(int position);

    Collection<ListNode<Atom>> getFirstOf();

    Atom getSubject(ListNode<Atom> notes);
}
