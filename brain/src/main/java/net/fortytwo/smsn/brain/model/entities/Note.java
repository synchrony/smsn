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
                new Property.Builder<Note, String>()
                        .key(SemanticSynchrony.PropertyKeys.ALIAS)
                        .getter(Note::getAlias)
                        .setter(Note::setAlias)
                        .fromString(s -> s)
                        .build(),
                new Property.Builder<Note, Long>()
                        .key(SemanticSynchrony.PropertyKeys.CREATED)
                        .isRequired(true)
                        .isSettable(false)
                        .getter(Note::getCreated)
                        .setter(Note::setCreated)
                        .fromString(Long::valueOf)
                        .build(),
                new Property.Builder<Note, String>()
                        .key(SemanticSynchrony.PropertyKeys.ID)
                        .isRequired(true)
                        .isSettable(false)
                        .isAnnotationProperty(false)
                        .getter(Note::getId)
                        .setter(Note::setId)
                        .fromString(s -> s)
                        .build(),
                new Property.Builder<Note, Float>()
                        .key(SemanticSynchrony.PropertyKeys.PRIORITY)
                        .isRequired(true)
                        .getter(Note::getPriority)
                        .setter(Note::setPriority)
                        .fromString(Float::valueOf)
                        .defaultValue(0f)
                        .build(),
                new Property.Builder<Note, Role>()
                        .key(SemanticSynchrony.PropertyKeys.ROLE)
                        .isRequired(true)
                        .getter(Note::getRole)
                        .setter(Note::setRole)
                        .fromString(Role::valueOf)
                        .defaultValue(Role.Entity)
                        .build(),
                new Property.Builder<Note, String>()
                        .key(SemanticSynchrony.PropertyKeys.SHORTCUT)
                        .getter(Note::getShortcut)
                        .setter(Note::setShortcut)
                        .fromString(s -> s)
                        .build(),
                new Property.Builder<Note, String>()
                        .key(SemanticSynchrony.PropertyKeys.SOURCE)
                        .isRequired(true)
                        .getter(Note::getSource)
                        .setter(Note::setSource)
                        .fromString(s -> s)
                        .build(),
                new Property.Builder<Note, String>()
                        .key(SemanticSynchrony.PropertyKeys.TEXT)
                        //.isAnnotationProperty(false)
                        .getter(Note::getText)
                        .setter(Note::setText)
                        .fromString(s -> s)
                        .build(),
                new Property.Builder<Note, String>()
                        .key(SemanticSynchrony.PropertyKeys.TITLE)
                        .isRequired(true)
                        .getter(Note::getTitle)
                        .setter(Note::setTitle)
                        .fromString(s -> s)
                        .build(),
                new Property.Builder<Note, Float>()
                        .key(SemanticSynchrony.PropertyKeys.WEIGHT)
                        .isRequired(true)
                        .getter(Note::getWeight)
                        .setter(Note::setWeight)
                        .fromString(Float::valueOf)
                        .defaultValue(0.5f)
                        .build(),
        }) {
            propertiesByKey.put(prop.getKey(), prop);
        }
        return propertiesByKey;
    }

    <V> V getProperty(String key);
    <V> void setProperty(String key, V value);

    Topic getTopic();

    void setTopic(Topic topic);

    ListNode<Note> getChildren();

    void setChildren(ListNode<Note> notes);

    void forFirstOf(Consumer<ListNode<Note>> consumer);

    void addChildAt(final Note child, int position);

    void deleteChildAt(int position);

    Collection<ListNode<Note>> getFirstOf();

    Note getSubject(ListNode<Note> notes);

    static String getId(Note note) {
        return note.getProperty(SemanticSynchrony.PropertyKeys.ID);
    }

    static void setId(Note note, String id) {
        note.setProperty(SemanticSynchrony.PropertyKeys.ID, id);
    }

    static Role getRole(Note note) {
        return note.getProperty(SemanticSynchrony.PropertyKeys.ROLE);
    }

    static void setRole(Note note, Role role) {
        note.setProperty(SemanticSynchrony.PropertyKeys.ROLE, role);
    }

    static String getAlias(Note note) {
        return note.getProperty(SemanticSynchrony.PropertyKeys.ALIAS);
    }

    static void setAlias(Note note, String alias) {
        note.setProperty(SemanticSynchrony.PropertyKeys.ALIAS, alias);
    }

    static Long getCreated(Note note) {
        return note.getProperty(SemanticSynchrony.PropertyKeys.CREATED);
    }

    static void setCreated(Note note, Long created) {
        note.setProperty(SemanticSynchrony.PropertyKeys.CREATED, created);
    }

    static String getTitle(Note note) {
        return note.getProperty(SemanticSynchrony.PropertyKeys.TITLE);
    }

    static void setTitle(Note note, String title) {
        note.setProperty(SemanticSynchrony.PropertyKeys.TITLE, title);
    }

    static String getText(Note note) {
        return note.getProperty(SemanticSynchrony.PropertyKeys.TEXT);
    }

    static void setText(Note note, String text) {
        note.setProperty(SemanticSynchrony.PropertyKeys.TEXT, text);
    }

    static Float getPriority(Note note) {
        return note.getProperty(SemanticSynchrony.PropertyKeys.PRIORITY);
    }

    static void setPriority(Note note, Float priority) {
        note.setProperty(SemanticSynchrony.PropertyKeys.PRIORITY, priority);
    }

    static String getShortcut(Note note) {
        return note.getProperty(SemanticSynchrony.PropertyKeys.SHORTCUT);
    }

    static void setShortcut(Note note, String shortcut) {
        note.setProperty(SemanticSynchrony.PropertyKeys.SHORTCUT, shortcut);
    }

    static Float getWeight(Note note) {
        return note.getProperty(SemanticSynchrony.PropertyKeys.WEIGHT);
    }

    static void setWeight(Note note, Float weight) {
        note.setProperty(SemanticSynchrony.PropertyKeys.WEIGHT, weight);
    }

    static String getSource(Note note) {
        return note.getProperty(SemanticSynchrony.PropertyKeys.SOURCE);
    }

    static void setSource(Note note, String source) {
        note.setProperty(SemanticSynchrony.PropertyKeys.SOURCE, source);
    }
}
