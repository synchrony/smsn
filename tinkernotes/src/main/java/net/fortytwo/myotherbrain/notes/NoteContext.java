package net.fortytwo.myotherbrain.notes;

import java.util.LinkedList;
import java.util.List;

/**
 * @author Joshua Shinavier (http://fortytwo.net)
 */
public class NoteContext extends NoteNode {
    private final List<Note> notes;
    private final List<NoteContext> children;

    public NoteContext(final String text) {
        super(text);
        this.notes = new LinkedList<Note>();
        this.children = new LinkedList<NoteContext>();
    }

    public List<Note> getNotes() {
        return notes;
    }

    public boolean addNote(final Note n) {
        return notes.add(n);
    }

    public List<NoteContext> getChildren() {
        return children;
    }

    public boolean addChild(final NoteContext c) {
        return children.add(c);
    }
}
