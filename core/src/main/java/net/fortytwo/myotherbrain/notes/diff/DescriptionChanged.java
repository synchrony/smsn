package net.fortytwo.myotherbrain.notes.diff;

/**
 * User: josh
 * Date: 6/13/11
 * Time: 11:08 PM
 */
public class DescriptionChanged {
    private final String atomId;
    private final String newDescription;

    public DescriptionChanged(String atomId, String newDescription) {
        this.atomId = atomId;
        this.newDescription = newDescription;
    }

    public String getAtomId() {
        return atomId;
    }

    public String getNewDescription() {
        return newDescription;
    }
}
