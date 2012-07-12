package net.fortytwo.myotherbrain.notes;

import java.util.LinkedList;
import java.util.List;

/**
 * @author Joshua Shinavier (http://fortytwo.net)
 */
public class Note {
    private final List<Note> children;

    private String value;
    private String id;
    private Float weight;
    private Float sharability;
    private Long created;
    private String alias = null;

    public Note() {
        children = new LinkedList<Note>();
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
        if (null != id && 0 == id.length()) {
            throw new IllegalArgumentException("empty id is not valid");
        }

        this.id = id;
    }

    public Float getWeight() {
        return weight;
    }

    public void setWeight(Float weight) {
        if (null != weight && (weight < 0 || weight > 1.0))  {
            throw new IllegalArgumentException("weight is out of range: " + weight);
        }

        this.weight = weight;
    }

    public Float getSharability() {
        return sharability;
    }

    public void setSharability(Float sharability) {
        if (null != sharability && (sharability < 0 || sharability > 1.0))  {
            throw new IllegalArgumentException("sharability is out of range: " + sharability);
        }

        this.sharability = sharability;
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

    public List<Note> getChildren() {
        return children;
    }

    public void addChild(Note child) {
        if (null == child) {
            throw new IllegalArgumentException("null child is not valid");
        }

        children.add(child);
    }

    @Override
    public String toString() {
        String d = getValue();
        if (null != d && d.length() > 20) {
            d = d.substring(0, 17) + "...";
        }
        return "note(" + d + ")";
    }
}
