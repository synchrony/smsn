package net.fortytwo.myotherbrain.notes;

/**
 * User: josh
 * Date: 5/18/11
 * Time: 6:13 PM
 */
public abstract class NoteNode {
    private final String text;
    protected String atomKey;
    protected String associationKey;

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

    public String getAtomKey() {
        return atomKey;
    }

    public String getAssociationKey() {
        return associationKey;
    }

    public void setAtomKey(final String atomKey) {
        this.atomKey = atomKey;
    }

    public void setAssociationKey(final String associationKey) {
        this.associationKey = associationKey;
    }
}
