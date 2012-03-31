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

    public Note() {
        children = new LinkedList<Note>();
    }

    public String getValue() {
        return value;
    }

    public void setValue(final String value) {
        this.value = value;
    }

    public String getId() {
        return id;
    }

    public void setId(final String id) {
        this.id = id;
    }

    public List<Note> getChildren() {
        return children;
    }

    public void addChild(Note child) {
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

    public Float getWeight() {
        return weight;
    }

    public void setWeight(Float weight) {
        this.weight = weight;
    }

    public Float getSharability() {
        return sharability;
    }

    public void setSharability(Float sharability) {
        this.sharability = sharability;
    }

    public Long getCreated() {
        return created;
    }

    public void setCreated(Long created) {
        this.created = created;
    }
}
