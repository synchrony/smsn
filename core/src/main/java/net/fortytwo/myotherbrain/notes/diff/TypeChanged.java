package net.fortytwo.myotherbrain.notes.diff;

/**
 * User: josh
 * Date: 6/13/11
 * Time: 11:11 PM
 */
public class TypeChanged {
    private final String atomId;
    private final String newType;

    public TypeChanged(final String atomId,
                       final String newType) {
        this.atomId = atomId;
        this.newType = newType;
    }

    public String getAtomId() {
        return atomId;
    }

    public String getNewType() {
        return newType;
    }
}
