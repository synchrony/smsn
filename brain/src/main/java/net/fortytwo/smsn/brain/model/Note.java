package net.fortytwo.smsn.brain.model;

import net.fortytwo.smsn.SemanticSynchrony;
import net.fortytwo.smsn.brain.model.entities.Atom;

import java.util.HashMap;
import java.util.LinkedList;
import java.util.List;
import java.util.Map;
import java.util.function.BiConsumer;
import java.util.function.Function;
import java.util.stream.Collectors;

public class Note {
    // A special value, for incoming notes only,
    // which causes an atom's alias or shortcut to be set to null (rather than merely ignored)
    public static final String CLEARME = "C.L.E.A.R.M.E";

    public static final Map<String, Property> propertiesByKey;

    static {
        propertiesByKey = new HashMap<>();
        for (Property prop : new Property[]{
                new Property<>(true,true, SemanticSynchrony.PropertyKeys.ALIAS,
                        Note::getAlias, Note::setAlias,
                        Atom::getAlias, Atom::setAlias, s -> s),
                new Property<>(false,true, SemanticSynchrony.PropertyKeys.CREATED,
                        Note::getCreated, Note::setCreated,
                        Atom::getCreated, Atom::setCreated, Long::valueOf),
                new Property<>(true,false, SemanticSynchrony.PropertyKeys.PAGE,
                        Note::getText, Note::setText,
                        Atom::getText, Atom::setText, s -> s),
                new Property<>(true,true, SemanticSynchrony.PropertyKeys.PRIORITY,
                        Note::getPriority, Note::setPriority,
                        Atom::getPriority, Atom::setPriority, Float::valueOf),
                new Property<>(true,true, SemanticSynchrony.PropertyKeys.SOURCE,
                        Note::getSource, Note::setSource,
                        Atom::getSource, Atom::setSource, s -> s),
                new Property<>(true,true, SemanticSynchrony.PropertyKeys.SHORTCUT,
                        Note::getShortcut, Note::setShortcut,
                        Atom::getShortcut, Atom::setShortcut, s -> s),
                new Property<>(true,false, SemanticSynchrony.PropertyKeys.TITLE,
                        Note::getTitle, Note::setTitle,
                        Atom::getTitle, Atom::setTitle, s -> s),
                new Property<>(true,true, SemanticSynchrony.PropertyKeys.WEIGHT,
                        Note::getWeight, Note::setWeight,
                        Atom::getWeight, Atom::setWeight, Float::valueOf),
        }) {
            propertiesByKey.put(prop.propertyKey, prop);
        }
    }

    private final List<Note> children;
    private int numberOfChildren;
    private int numberOfParents;
    private String title;
    private String text;
    private String id;
    private Float weight;
    private Float priority;
    private Long created;
    private String alias;
    private String shortcut;
    private String source;
    //private String type;
    private List<String> meta;

    public Note() {
        children = new LinkedList<>();
    }

    public Note(final Note copy) {
        this();

        this.title = copy.title;
        this.text = copy.text;
        this.id = copy.id;
        this.weight = copy.weight;
        this.priority = copy.priority;
        this.created = copy.created;
        this.alias = copy.alias;

        this.numberOfChildren = copy.numberOfChildren;
        this.numberOfParents = copy.numberOfParents;
        //this.type = copy.type;
        this.meta = copy.meta;

        if (null != copy.children) {
            children.addAll(copy.children.stream().map(Note::new).collect(Collectors.toList()));
        }
    }

    public String getTitle() {
        return title;
    }

    public void setTitle(final String title) {
        this.title = title;
    }

    public String getText() {
        return text;
    }

    public void setText(String text) {
        this.text = text;
    }

    public String getId() {
        return id;
    }

    public void setId(final String id) {
        // null ids are frequently used, but empty ids would be likely to cause confusion
        if (null != id && 0 == id.length()) {
            throw new IllegalArgumentException("empty id is not valid");
        }

        this.id = id;
    }

    public Float getPriority() {
        return priority;
    }

    public void setPriority(Float priority) {
        if (null != priority && (priority < 0.0 || priority > 1.0)) {
            throw new IllegalArgumentException("priority is out of range: " + priority);
        }

        this.priority = priority;
    }

    public Float getWeight() {
        return weight;
    }

    public void setWeight(Float weight) {
        if (null != weight && (weight < 0.0 || weight > 1.0)) {
            throw new IllegalArgumentException("weight is out of range: " + weight);
        }

        this.weight = weight;
    }
    
    public String getSource() {
        return source;
    }

    public void setSource(String source) {
        this.source = source;
    }
    
    public Long getCreated() {
        return created;
    }

    public void setCreated(Long created) {
        this.created = created;
    }

    public String getAlias() {
        return alias;
    }

    public void setAlias(String alias) {
        if (null != alias && 0 == alias.length()) {
            throw new IllegalArgumentException("empty alias is not valid");
        }

        this.alias = alias;
    }

    public String getShortcut() {
        return shortcut;
    }

    public void setShortcut(final String shortcut) {
        if (null != shortcut && 0 == shortcut.length()) {
            throw new IllegalArgumentException("empty shortcut is not valid");
        }

        this.shortcut = shortcut;
    }

    public List<String> getMeta() {
        return meta;
    }

    public void setMeta(List<String> meta) {
        this.meta = meta;
    }

    public List<Note> getChildren() {
        return children;
    }

    public void addChild(Note child) {
        if (null == child) {
            throw new IllegalArgumentException("null child is not valid");
        }

        children.add(child);
    }

    public int getNumberOfChildren() {
        return numberOfChildren;
    }

    public void setNumberOfChildren(int numberOfChildren) {
        this.numberOfChildren = numberOfChildren;
    }

    public int getNumberOfParents() {
        return numberOfParents;
    }

    public void setNumberOfParents(int numberOfParents) {
        this.numberOfParents = numberOfParents;
    }

    @Override
    public String toString() {
        return "note[" + (null == id ? "null" : id) + "]";
    }

    public static class Property<T> {
        private final boolean isSettable;
        private final boolean isAnnotationProperty;
        private final String propertyKey;
        private final Function<Note, T> noteGetter;
        private final BiConsumer<Note, T> noteSetter;
        private final Function<Atom, T> atomGetter;
        private final BiConsumer<Atom, T> atomSetter;
        private final Function<String, T> fromString;

        private Property(final boolean isSettable,
                         final boolean isAnnotationProperty,
                         final String propertyKey,
                         final Function<Note, T> noteGetter,
                         final BiConsumer<Note, T> noteSetter,
                         final Function<Atom, T> atomGetter,
                         final BiConsumer<Atom, T> atomSetter,
                         final Function<String, T> fromString) {
            this.isSettable = isSettable;
            this.isAnnotationProperty = isAnnotationProperty;
            this.propertyKey = propertyKey;
            this.noteGetter = noteGetter;
            this.noteSetter = noteSetter;
            this.atomGetter = atomGetter;
            this.atomSetter = atomSetter;
            this.fromString = fromString;
        }

        public boolean isAnnotationProperty() {
            return isAnnotationProperty;
        }

        public String getPropertyKey() {
            return propertyKey;
        }

        public Function<Note, T> getNoteGetter() {
            return noteGetter;
        }

        public BiConsumer<Note, T> getNoteSetter() {
            return noteSetter;
        }

        public Function<Atom, T> getAtomGetter() {
            return atomGetter;
        }

        public BiConsumer<Atom, T> getAtomSetter() {
            return atomSetter;
        }

        public Function<String, T> getFromString() {
            return fromString;
        }

        public boolean isSettable() {
            return isSettable;
        }
    }
}
