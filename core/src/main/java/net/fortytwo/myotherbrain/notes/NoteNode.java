package net.fortytwo.myotherbrain.notes;

/**
 * User: josh
 * Date: 5/18/11
 * Time: 6:13 PM
 */
public abstract class NoteNode {
    private final String text;
    protected String atomId;
    protected String associationId;

    public NoteNode(final String text) {
        this.text = text;

        //if (null == text) {
        //    throw new IllegalArgumentException("note text must be non-null" +
        //            " (although it may be an empty string)");
        //}
    }

    public String getText() {
        return text;
    }

    public String getAtomId() {
        return atomId;
    }

    public String getAssociationId() {
        return associationId;
    }

    public void setAtomId(String atomId) {
        this.atomId = atomId;
    }

    public void setAssociationId(String associationId) {
        this.associationId = associationId;
    }
}
