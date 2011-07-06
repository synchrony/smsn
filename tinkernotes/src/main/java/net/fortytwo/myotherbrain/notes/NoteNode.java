package net.fortytwo.myotherbrain.notes;

/**
 * @author Joshua Shinavier (http://fortytwo.net)
 */
public abstract class NoteNode {
    private final String targetValue;
    protected String targetKey;
    protected String linkKey;

    public NoteNode(final String targetValue) {
        this.targetValue = targetValue;

        //if (null == text) {
        //    throw new IllegalArgumentException("note text must be non-null" +
        //            " (although it may be an empty string)");
        //}
    }

    public String getTargetValue() {
        return targetValue;
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
