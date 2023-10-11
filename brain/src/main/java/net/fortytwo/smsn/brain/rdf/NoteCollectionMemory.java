package net.fortytwo.smsn.brain.rdf;

import net.fortytwo.smsn.brain.AtomId;
import net.fortytwo.smsn.brain.model.entities.Note;

import java.util.Collection;
import java.util.LinkedList;

public class NoteCollectionMemory {

    private final AtomId noteId;
    private final Collection<NoteCollectionMemory> collections = new LinkedList<>();
    private final Collection<Note> notes = new LinkedList<>();

    public NoteCollectionMemory(final AtomId noteId) {
        this.noteId = noteId;
    }

    public AtomId getNoteId() {
        return noteId;
    }

    public Collection<NoteCollectionMemory> getMemberCollections() {
        return collections;
    }

    public Collection<Note> getMemberNotes() {
        return notes;
    }
}
