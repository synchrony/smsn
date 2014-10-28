package net.fortytwo.extendo.brain;

import java.util.LinkedList;
import java.util.List;

/**
 * @author Joshua Shinavier (http://fortytwo.net)
 */
public class Note {
    // A special value, for incoming notes only, which causes an atom's alias to be set to null (rather than merely ignored)
    public static final String CLEAR_ALIAS = "_";

    private final List<Note> children;
    private boolean hasChildren;
    private String value;
    private String id;
    private Float weight;
    private Float sharability;
    private Float priority;
    private Long created;
    private String alias;
    //private String type;
    private List<String> meta;

    public Note() {
        children = new LinkedList<Note>();
    }

    public Note(final Note copy) {
        this();

        this.value = copy.value;
        this.id = copy.id;
        this.weight = copy.weight;
        this.sharability = copy.sharability;
        this.priority = copy.priority;
        this.created = copy.created;
        this.alias = copy.alias;

        this.hasChildren = copy.hasChildren;
        //this.type = copy.type;
        this.meta = copy.meta;

        if (null != copy.children) {
            for (Note c : copy.children) {
                children.add(new Note(c));
            }
        }
    }

    public String getValue() {
        return value;
    }

    public void setValue(final String value) {
        if (null != value && 0 == value.length()) {
            throw new IllegalArgumentException("empty value is not valid");
        }

        this.value = value;
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
        if (null != priority && (priority < 0 || priority > 1.0)) {
            throw new IllegalArgumentException("priority is out of range: " + priority);
        }

        this.priority = priority;
    }

    public Float getSharability() {
        return sharability;
    }

    public void setSharability(Float sharability) {
        if (null != sharability && (sharability <= 0 || sharability > 1.0)) {
            throw new IllegalArgumentException("sharability is out of range: " + sharability);
        }

        this.sharability = sharability;
    }

    public Float getWeight() {
        return weight;
    }

    public void setWeight(Float weight) {
        if (null != weight && (weight <= 0 || weight > 1.0)) {
            throw new IllegalArgumentException("weight is out of range: " + weight);
        }

        this.weight = weight;
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

    /*
    public String getType() {
        return type;
    }

    public void setType(final String type) {
        this.type = type;
    }
    */

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
        hasChildren = true;
    }

    @Override
    public String toString() {
        String d = getValue();
        if (null != d && d.length() > 20) {
            d = d.substring(0, 17) + "...";
        }
        return "note(" + d + ")";
    }

    public boolean getHasChildren() {
        return hasChildren;
    }

    public void setHasChildren(final boolean b) {
        hasChildren = b;
    }

    // note: deliberately leaves hasChildren unaffected
    public void truncate(final int depth) {
        if (depth <= 1) {
            children.clear();
        } else {
            for (Note c : children) {
                c.truncate(depth - 1);
            }
        }
    }
}
