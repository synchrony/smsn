package net.fortytwo.smsn.brain.model.entities;

import net.fortytwo.smsn.SemanticSynchrony;
import net.fortytwo.smsn.brain.model.Property;
import net.fortytwo.smsn.brain.model.Role;

import java.util.LinkedHashMap;
import java.util.Map;
import java.util.Stack;
import java.util.function.Consumer;

public interface Note extends ListNode<Note> {

    Map<String, Property<Note, ?>> propertiesByKey = createPropertiesByKey();

    static Map<String, Property<Note, ?>> createPropertiesByKey() {
        Map<String, Property<Note, ?>> propertiesByKey = new LinkedHashMap<>();
        for (Property<Note, ?> prop : new Property[]{
                new Property.Builder<Note, String>(String.class)
                        .key(SemanticSynchrony.PropertyKeys.ALIAS)
                        .getter(Note::getAlias)
                        .setter(Note::setAlias)
                        .fromString(s -> s)
                        .build(),
                new Property.Builder<Note, Long>(Long.class)
                        .key(SemanticSynchrony.PropertyKeys.CREATED)
                        //.isRequired(true) RESTORE ME
                        .isSettable(false)
                        .getter(Note::getCreated)
                        .setter(Note::setCreated)
                        .fromString(Long::valueOf)
                        .build(),
                new Property.Builder<Note, String>(String.class)
                        .key(SemanticSynchrony.PropertyKeys.LABEL)
                        //.isRequired(true) RESTORE ME
                        .getter(Note::getLabel)
                        .setter(Note::setLabel)
                        .fromString(s -> s)
                        .validator(value -> {
                            if (value.trim().length() == 0) {
                                throw new IllegalArgumentException("empty value");
                            }
                        })
                        .build(),
                new Property.Builder<Note, Float>(Float.class)
                        .key(SemanticSynchrony.PropertyKeys.PRIORITY)
                        //.isRequired(true) RESTORE ME
                        .getter(Note::getPriority)
                        .setter(Note::setPriority)
                        .fromString(Float::valueOf)
                        .defaultValue(0f)
                        .validator(value -> {
                            if (value < 0 || value > 1.0) {
                                throw new IllegalArgumentException("priority is outside of range [0, 1]: " + value);
                            }
                        })
                        .build(),
                new Property.Builder<Note, Role>(Role.class)
                        .key(SemanticSynchrony.PropertyKeys.ROLE)
                        //.isRequired(true) RESTORE ME
                        .getter(Note::getRole)
                        .setter(Note::setRole)
                        .fromString(Role::valueOf)
                        .defaultValue(Role.Entity)
                        .build(),
                new Property.Builder<Note, String>(String.class)
                        .key(SemanticSynchrony.PropertyKeys.SHORTCUT)
                        .getter(Note::getShortcut)
                        .setter(Note::setShortcut)
                        .fromString(s -> s)
                        .validator(value -> {
                            if (value.length() > 50) {
                                throw new IllegalArgumentException("shortcut is too long: " + value);
                            }
                        })
                        .build(),
                new Property.Builder<Note, String>(String.class)
                        .key(SemanticSynchrony.PropertyKeys.SOURCE)
                        //.isRequired(true) RESTORE ME
                        .getter(Note::getSource)
                        .setter(Note::setSource)
                        .fromString(s -> s)
                        .validator(value -> {
                            if (value.trim().length() == 0) {
                                throw new IllegalArgumentException("empty source");
                            }
                        })
                        .build(),
                new Property.Builder<Note, String>(String.class)
                        .key(SemanticSynchrony.PropertyKeys.TEXT)
                        //.isAnnotationProperty(false)
                        .getter(Note::getText)
                        .setter(Note::setText)
                        .fromString(s -> s)
                        .build(),
                new Property.Builder<Note, Float>(Float.class)
                        .key(SemanticSynchrony.PropertyKeys.WEIGHT)
                        //.isRequired(true) RESTORE ME
                        .getter(Note::getWeight)
                        .setter(Note::setWeight)
                        .fromString(Float::valueOf)
                        .defaultValue(0.5f)
                        .validator(value -> {
                            // Note: weight may not currently be set to 0, which would cause the note to disappear from all normal views
                            if (value <= 0 || value > 1.0) {
                                throw new IllegalArgumentException("weight is outside of range (0, 1]: " + value);
                            }
                        })
                        .build(),
        }) {
            propertiesByKey.put(prop.getKey(), prop);
        }
        return propertiesByKey;
    }

    default void setSource(String source) {
        setProperty(SemanticSynchrony.PropertyKeys.SOURCE, source);
    }

    default String getSource() {
        return getProperty(SemanticSynchrony.PropertyKeys.SOURCE);
    }

    default void setWeight(Float weight) {
        setProperty(SemanticSynchrony.PropertyKeys.WEIGHT, weight);
    }

    default Float getWeight() {
        return getProperty(SemanticSynchrony.PropertyKeys.WEIGHT);
    }

    default void setShortcut(String shortcut) {
        setProperty(SemanticSynchrony.PropertyKeys.SHORTCUT, shortcut);
    }

    default String getShortcut() {
        return getProperty(SemanticSynchrony.PropertyKeys.SHORTCUT);
    }

    default void setPriority(Float priority) {
        setProperty(SemanticSynchrony.PropertyKeys.PRIORITY, priority);
    }

    default Float getPriority() {
        return getProperty(SemanticSynchrony.PropertyKeys.PRIORITY);
    }

    default void setText(String text) {
        setProperty(SemanticSynchrony.PropertyKeys.TEXT, text);
    }

    default String getText() {
        return getProperty(SemanticSynchrony.PropertyKeys.TEXT);
    }

    default void setLabel(String title) {
        setProperty(SemanticSynchrony.PropertyKeys.LABEL, title);
    }

    default String getLabel() {
        return getProperty(SemanticSynchrony.PropertyKeys.LABEL);
    }

    default void setCreated(Long created) {
        setProperty(SemanticSynchrony.PropertyKeys.CREATED, created);
    }

    default Long getCreated() {
        return getProperty(SemanticSynchrony.PropertyKeys.CREATED);
    }

    default void setAlias(String alias) {
        setProperty(SemanticSynchrony.PropertyKeys.ALIAS, alias);
    }

    default String getAlias() {
        return getProperty(SemanticSynchrony.PropertyKeys.ALIAS);
    }

    default void setRole(Role role) {
        setProperty(SemanticSynchrony.PropertyKeys.ROLE, role);
    }

    default Role getRole() {
        return getProperty(SemanticSynchrony.PropertyKeys.ROLE);
    }

    <V> V getProperty(String key);
    <V> void setProperty(String key, V value);

    int getNumberOfChildren();
    int getNumberOfParents();

    Topic getTopic();

    void setTopic(Topic topic);

    Note getParent();

    void addChild(int index, Note child);

    void removeChild(int index);

    static void setChildren(final Note parent, Stack<Note> children) {
        if (children.isEmpty()) {
            return;
        }

        Note cur = null;
        while (!children.isEmpty()) {
            Note next = children.pop();
            next.setRest(cur);
            cur = next;
        }
        parent.setFirst(cur);
    }

    static void setChildren(final Note parent, Note... children) {
        setChildren(parent, toStack(children));
    }

    static <T> Stack<T> toStack(final T... values) {
        Stack<T> stack = new Stack<>();
        for (int i = values.length - 1; i >= 0; i--) {
            stack.push(values[i]);
        }
        return stack;
    }

    static void copyProperties(final Note from, final Note to) {
        for (Property prop : propertiesByKey.values()) {
            prop.getSetter().accept(to, prop.getGetter().apply(from));
        }
    }

    static void forAllChildren(final Note note, final Consumer<Note> consumer) {
        Note child = note.getFirst();
        while (null != child) {
            consumer.accept(child);
            child = (Note) child.getRest();
        }
    }

    static boolean isRoot(final Note note) {
        return null == note.getParent();
    }
}
