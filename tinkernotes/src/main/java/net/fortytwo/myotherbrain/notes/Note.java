package net.fortytwo.myotherbrain.notes;

import java.util.LinkedList;
import java.util.List;

/**
 * @author Joshua Shinavier (http://fortytwo.net)
 */
public class Note {
    private final List<Note> children;

    private String targetValue;
    private String targetKey;
    private Float targetWeight;
    private Float targetSharability;
    private Long targetCreated;

    public Note() {
        children = new LinkedList<Note>();
    }

    public String getTargetValue() {
        return targetValue;
    }

    public void setTargetValue(final String targetValue) {
        this.targetValue = targetValue;
    }

    public String getTargetKey() {
        return targetKey;
    }

    public void setTargetKey(final String targetKey) {
        this.targetKey = targetKey;
    }

    public List<Note> getChildren() {
        return children;
    }

    public void addChild(Note child) {
        children.add(child);
    }

    @Override
    public String toString() {
        String d = getTargetValue();
        if (null != d && d.length() > 20) {
            d = d.substring(0, 17) + "...";
        }
        return "note(" + d + ")";
    }

    public Float getTargetWeight() {
        return targetWeight;
    }

    public void setTargetWeight(Float targetWeight) {
        this.targetWeight = targetWeight;
    }

    public Float getTargetSharability() {
        return targetSharability;
    }

    public void setTargetSharability(Float targetSharability) {
        this.targetSharability = targetSharability;
    }

    public Long getTargetCreated() {
        return targetCreated;
    }

    public void setTargetCreated(Long targetCreated) {
        this.targetCreated = targetCreated;
    }
}
