package net.fortytwo.myotherbrain.notes.diff;

/**
 * User: josh
 * Date: 6/13/11
 * Time: 11:25 PM
 */
public class NoteRemoved {
    private final String associationId;

    public NoteRemoved(String associationId) {
        this.associationId = associationId;

        if (null == associationId) {
            throw new IllegalArgumentException();
        }
    }

    public String getAssociationId() {
        return associationId;
    }
}
