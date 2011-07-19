package net.fortytwo.myotherbrain.notes;

import java.util.LinkedList;
import java.util.List;

/**
 * @author Joshua Shinavier (http://fortytwo.net)
 */
public class Note extends NoteNode {
    private final List<Note> children;
    private String qualifier;
    private String linkValue;
    private boolean meta = false;

    private Float linkWeight;
    private Float linkSharability;
    private Long linkCreated;
    private Float targetWeight;
    private Float targetSharability;
    private Long targetCreated;

    public Note(final String targetValue) {
        super(targetValue);

        children = new LinkedList<Note>();
    }

    public String getLinkValue() {
        return linkValue;
    }

    public void setLinkValue(String linkValue) {
        this.linkValue = linkValue;
    }

    public List<Note> getChildren() {
        return children;
    }

    public void addChild(Note child) {
        children.add(child);
    }

    public String getQualifier() {
        return qualifier;
    }

    public void setQualifier(String qualifier) {
        this.qualifier = qualifier;
    }

    @Override
    public String toString() {
        String d = getTargetValue();
        if (null != d && d.length() > 20) {
            d = d.substring(0, 17) + "...";
        }
        String t = getLinkValue();
        if (null != t && t.length() > 20) {
            t = t.substring(0, 17) + "...";
        }
        return "note(" + t + ", " + d + ")";
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

    public Float getLinkWeight() {
        return linkWeight;
    }

    public void setLinkWeight(Float linkWeight) {
        this.linkWeight = linkWeight;
    }

    public Float getLinkSharability() {
        return linkSharability;
    }

    public void setLinkSharability(Float linkSharability) {
        this.linkSharability = linkSharability;
    }

    public Long getLinkCreated() {
        return linkCreated;
    }

    public void setLinkCreated(Long linkCreated) {
        this.linkCreated = linkCreated;
    }

    public Long getTargetCreated() {
        return targetCreated;
    }

    public void setTargetCreated(Long targetCreated) {
        this.targetCreated = targetCreated;
    }

    public boolean isMeta() {
        return meta;
    }

    public void setMeta(boolean meta) {
        this.meta = meta;
    }
}
