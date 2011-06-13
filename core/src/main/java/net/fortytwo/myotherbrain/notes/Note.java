package net.fortytwo.myotherbrain.notes;

import java.util.LinkedList;
import java.util.List;

/**
 * User: josh
 * Date: 5/18/11
 * Time: 5:51 PM
 */
public class Note extends NoteNode {
    private final String type;
    private final List<Note> children;
    private String qualifier;
    private Id id;

    public Note(final String type,
                final String text) {
        super(text);

        this.type = type;
        children = new LinkedList<Note>();
    }

    public String getType() {
        return type;
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

    public Id getId() {
        return id;
    }

    public void setId(Id id) {
        this.id = id;
    }

    public static class Id {
        private final String atomId;
        private final String associationId;

        public Id(String atomId, String associationId) {
            this.atomId = atomId;
            this.associationId = associationId;
        }

        public String getAtomId() {
            return atomId;
        }

        public String getAssociationId() {
            return associationId;
        }
    }
}
