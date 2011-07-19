package net.fortytwo.myotherbrain.notes;

/**
 * @author Joshua Shinavier (http://fortytwo.net)
 */
public abstract class NoteNode {
    protected String targetValue;
    protected String targetKey;
    protected String linkKey;

    public String getTargetValue() {
        return targetValue;
    }

    public void setTargetValue(final String targetValue) {
        this.targetValue = targetValue;
    }

    public String getTargetKey() {
        return targetKey;
    }

    public String getLinkKey() {
        return linkKey;
    }

    public void setTargetKey(final String targetKey) {
        this.targetKey = targetKey;
    }

    public void setLinkKey(final String linkKey) {
        this.linkKey = linkKey;
    }
}
