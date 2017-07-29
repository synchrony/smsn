package net.fortytwo.smsn.brain.model.entities;

import net.fortytwo.smsn.SemanticSynchrony;
import net.fortytwo.smsn.brain.model.Property;
import net.fortytwo.smsn.brain.model.Role;

import java.util.Collection;
import java.util.LinkedHashMap;
import java.util.Map;
import java.util.function.Consumer;

public interface Note extends Entity {

    Map<String, Property<Note, ?>> propertiesByKey = createPropertiesByKey();

    static Map<String, Property<Note, ?>> createPropertiesByKey() {
        Map<String, Property<Note, ?>> propertiesByKey = new LinkedHashMap<>();
        for (Property<Note, ?> prop : new Property[]{
                new Property.Builder<Note, Role>()
                        .key(SemanticSynchrony.PropertyKeys.ROLE)
                        .getter(Note::getRole)
                        .setter(Note::setRole)
                        .fromString(Role::valueOf)
                        .defaultValue(Role.Entity)
                        .build(),
                new Property.Builder<Note, String>()
                        .key(SemanticSynchrony.PropertyKeys.ALIAS)
                        .getter(Note::getAlias)
                        .setter(Note::setAlias)
                        .fromString(s -> s)
                        .build(),
                new Property.Builder<Note, Long>()
                        .key(SemanticSynchrony.PropertyKeys.CREATED)
                        .isSettable(false)
                        .getter(Note::getCreated)
                        .setter(Note::setCreated)
                        .fromString(Long::valueOf)
                        .build(),
                new Property.Builder<Note, String>()
                        .key(SemanticSynchrony.PropertyKeys.TEXT)
                        .isAnnotationProperty(false)
                        .getter(Note::getText)
                        .setter(Note::setText)
                        .fromString(s -> s)
                        .build(),
                new Property.Builder<Note, String>()
                        .key(SemanticSynchrony.PropertyKeys.SOURCE)
                        .getter(Note::getSource)
                        .setter(Note::setSource)
                        .fromString(s -> s)
                        .build(),
                new Property.Builder<Note, String>()
                        .key(SemanticSynchrony.PropertyKeys.SHORTCUT)
                        .getter(Note::getShortcut)
                        .setter(Note::setShortcut)
                        .fromString(s -> s)
                        .build(),
                new Property.Builder<Note, String>()
                        .key(SemanticSynchrony.PropertyKeys.TITLE)
                        .getter(Note::getTitle)
                        .setter(Note::setTitle)
                        .fromString(s -> s)
                        .build(),
                new Property.Builder<Note, Float>()
                        .key(SemanticSynchrony.PropertyKeys.WEIGHT)
                        .getter(Note::getWeight)
                        .setter(Note::setWeight)
                        .fromString(Float::valueOf)
                        .defaultValue(0.5f)
                        .build(),
                new Property.Builder<Note, Float>()
                        .key(SemanticSynchrony.PropertyKeys.PRIORITY)
                        .getter(Note::getPriority)
                        .setter(Note::setPriority)
                        .fromString(Float::valueOf)
                        .defaultValue(0.5f)
                        .build()
        }) {
            propertiesByKey.put(prop.getKey(), prop);
        }
        return propertiesByKey;
    }

    String getId();

    void setId(String id);

    Role getRole();

    void setRole(Role role);

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
