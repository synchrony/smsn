package net.fortytwo.myotherbrain.notes.diff;

/**
 * User: josh
 * Date: 6/13/11
 * Time: 11:08 PM
 */
public class NameChanged {
    private final String atomId;
    private final String newName;

    public NameChanged(final String atomId,
                       final String newName) {
        this.atomId = atomId;
        this.newName = newName;
    }

    public String getAtomId() {
        return atomId;
    }

    public String getNewName() {
        return newName;
    }
}
