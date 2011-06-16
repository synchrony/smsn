package net.fortytwo.myotherbrain.notes.diff;

/**
 * User: josh
 * Date: 6/13/11
 * Time: 11:25 PM
 */
public class NoteAdded {
    private final String atomId;
    private final String associationId;

    public NoteAdded(final String atomId,
                     final String associationId) {
        this.atomId = atomId;
        this.associationId = associationId;

        if (null == atomId || null == associationId) {
            throw new IllegalArgumentException();
        }
    }

    public String getAtomId() {
        return atomId;
    }

    public String getAssociationId() {
        return associationId;
    }
}
