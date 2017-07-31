package net.fortytwo.smsn.brain.rdf;

import net.fortytwo.smsn.brain.model.entities.Note;

import java.util.Collection;
import java.util.LinkedList;

public class NoteCollectionMemory {

    private final String noteId;
    private final Collection<NoteCollectionMemory> collections = new LinkedList<>();
    private final Collection<Note> notes = new LinkedList<>();

    public NoteCollectionMemory(final String noteId) {
        this.noteId = noteId;
    }

    public String getNoteId() {
        return noteId;
    }

    public Collection<NoteCollectionMemory> getMemberCollections() {
        return collections;
    }

    public Collection<Note> getMemberNotes() {
        return notes;
    }
}
