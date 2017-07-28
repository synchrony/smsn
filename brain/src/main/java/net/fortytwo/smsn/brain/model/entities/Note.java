package net.fortytwo.smsn.brain.model.entities;

import net.fortytwo.smsn.SemanticSynchrony;
import net.fortytwo.smsn.brain.model.Property;
import net.fortytwo.smsn.brain.model.Tag;

import java.util.Collection;
import java.util.LinkedHashMap;
import java.util.Map;
import java.util.function.Consumer;

public interface Note extends Entity {

    Map<String, Property<Note, ?>> propertiesByKey = createPropertiesByKey();

    static Map<String, Property<Note, ?>> createPropertiesByKey() {
        Map<String, Property<Note, ?>> propertiesByKey = new LinkedHashMap<>();
        for (Property<Note, ?> prop : new Property[]{
                new Property<>(true, true, SemanticSynchrony.PropertyKeys.TAG,
                        Note::getTag, Note::setTag, Tag::valueOf),
                new Property<>(true, true, SemanticSynchrony.PropertyKeys.ALIAS,
                        Note::getAlias, Note::setAlias, s -> s),
                new Property<>(true, true, SemanticSynchrony.PropertyKeys.CREATED,
                        Note::getCreated, Note::setCreated, Long::valueOf),
                new Property<>(true, false, SemanticSynchrony.PropertyKeys.TEXT,
                        Note::getText, Note::setText, s -> s),
                new Property<>(true, true, SemanticSynchrony.PropertyKeys.SOURCE,
                        Note::getSource, Note::setSource, s -> s),
                new Property<>(true, true, SemanticSynchrony.PropertyKeys.SHORTCUT,
                        Note::getShortcut, Note::setShortcut, s -> s),
                new Property<>(true, true, SemanticSynchrony.PropertyKeys.TITLE,
                        Note::getTitle, Note::setTitle, s -> s),
                new Property<>(true, true, SemanticSynchrony.PropertyKeys.WEIGHT,
                        Note::getWeight, Note::setWeight, Float::valueOf),
                new Property<>(true, true, SemanticSynchrony.PropertyKeys.PRIORITY,
                        Note::getPriority, Note::setPriority, Float::valueOf),
        }) {
            propertiesByKey.put(prop.getPropertyKey(), prop);
        }
        return propertiesByKey;
    }

    String getId();

    void setId(String id);

    Tag getTag();

    void setTag(Tag tag);

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

    Topic getTopic();

    void setTopic(Topic topic);

    ListNode<Note> getChildren();

    void setChildren(ListNode<Note> notes);

    void forFirstOf(Consumer<ListNode<Note>> consumer);

    void addChildAt(final Note child, int position);

    void deleteChildAt(int position);

    Collection<ListNode<Note>> getFirstOf();

    Note getSubject(ListNode<Note> notes);
}
