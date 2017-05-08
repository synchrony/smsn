package net.fortytwo.smsn.brain.model;

import net.fortytwo.smsn.SemanticSynchrony;

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
    public static final String CLEARME = "_";

    public static final Map<String, Property> propertiesByKey;

    static {
        propertiesByKey = new HashMap<>();
        for (Property prop : new Property[]{
                new Property<>(true, SemanticSynchrony.PropertyKeys.ALIAS,
                        Note::getAlias, Note::setAlias, s -> s),
                new Property<>(true, SemanticSynchrony.PropertyKeys.CREATED,
                        Note::getCreated, Note::setCreated, Long::valueOf),
                new Property<>(false, SemanticSynchrony.PropertyKeys.PAGE,
                        Note::getPage, Note::setPage, s -> s),
                new Property<>(true, SemanticSynchrony.PropertyKeys.PRIORITY,
                        Note::getPriority, Note::setPriority, Float::valueOf),
                new Property<>(true, SemanticSynchrony.PropertyKeys.SHARABILITY,
                        Note::getSharability, Note::setSharability, Float::valueOf),
                new Property<>(true, SemanticSynchrony.PropertyKeys.SOURCE,
                        Note::getSource, Note::setSource, s -> s),
                new Property<>(true, SemanticSynchrony.PropertyKeys.SHORTCUT,
                        Note::getShortcut, Note::setShortcut, s -> s),
                new Property<>(false, SemanticSynchrony.PropertyKeys.TITLE,
                        Note::getTitle, Note::setTitle, s -> s),
                new Property<>(true, SemanticSynchrony.PropertyKeys.WEIGHT,
                        Note::getWeight, Note::setWeight, Float::valueOf),
        }) {
            propertiesByKey.put(prop.propertyKey, prop);
        }
    }

    private final List<Note> children;
    private int numberOfChildren;
    private int numberOfParents;
    private String title;
    private String page;
    private String id;
    private Float weight;
    private Float sharability;
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
        this.page = copy.page;
        this.id = copy.id;
        this.weight = copy.weight;
        this.sharability = copy.sharability;
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

    public String getPage() {
        return page;
    }

    public void setPage(String page) {
        this.page = page;
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

    public Float getSharability() {
        return sharability;
    }

    public void setSharability(Float sharability) {
        if (null != sharability && (sharability < 0.0 || sharability > 1.0)) {
            throw new IllegalArgumentException("sharability is out of range: " + sharability);
        }

        this.sharability = sharability;
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
        private final boolean isAnnotationProperty;
        private final String propertyKey;
        private final Function<Note, T> getter;
        private final BiConsumer<Note, T> setter;
        private final Function<String, T> fromString;

        private Property(final boolean isAnnotationProperty,
                         final String propertyKey,
                         final Function<Note, T> getter,
                         final BiConsumer<Note, T> setter,
                         final Function<String, T> fromString) {
            this.isAnnotationProperty = isAnnotationProperty;
            this.propertyKey = propertyKey;
            this.getter = getter;
            this.setter = setter;
            this.fromString = fromString;
        }

        public boolean isAnnotationProperty() {
            return isAnnotationProperty;
        }

        public String getPropertyKey() {
            return propertyKey;
        }

        public Function<Note, T> getGetter() {
            return getter;
        }

        public BiConsumer<Note, T> getSetter() {
            return setter;
        }

        public Function<String, T> getFromString() {
            return fromString;
        }
    }
}
