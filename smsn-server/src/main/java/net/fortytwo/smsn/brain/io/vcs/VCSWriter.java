package net.fortytwo.smsn.brain.io.vcs;

import net.fortytwo.smsn.brain.io.BrainWriter;
import net.fortytwo.smsn.brain.io.Format;
import net.fortytwo.smsn.brain.model.Atom;
import net.fortytwo.smsn.brain.model.AtomGraph;
import net.fortytwo.smsn.brain.model.AtomList;
import net.fortytwo.smsn.brain.model.Note;
import net.fortytwo.smsn.brain.wiki.NoteWriter;
import org.apache.commons.io.FileUtils;

import java.io.File;
import java.io.FileOutputStream;
import java.io.IOException;
import java.io.OutputStream;
import java.util.LinkedList;
import java.util.List;

public class VCSWriter extends BrainWriter {
    private static final List<Format> formats;
    private static final NoteWriter noteWriter = new NoteWriter();

    static {
        formats = new LinkedList<>();
        formats.add(new Format("VCS", Format.Type.DirectoryBased));
    }

    @Override
    public List<Format> getFormats() {
        return formats;
    }

    @Override
    public void doExport(Context context) throws IOException {
        File dir = context.getDestDirectory();
        timeIt("cleared directory", () -> clearDirectory(dir));

        timeIt("exported atoms as individual files", () -> doExport(context.getAtomGraph(), dir));
    }

    private void clearDirectory(final File dir) throws IOException {
        FileUtils.cleanDirectory(dir);
    }

    private void doExport(final AtomGraph graph, final File dir) throws IOException {
        for (Atom a : graph.getAllAtoms()) {
            File atomFile = new File(dir, "atom" + a.getId() + ".txt");
            try (OutputStream out = new FileOutputStream(atomFile)) {
                writeAtomToStream(a, out);
            }
        }
    }

    private void writeAtomToStream(final Atom atom, final OutputStream out) {
        Note note = toNote(atom, true);
        AtomList list = atom.getNotes();
        while (null != list) {
            note.getChildren().add(toNote(list.getFirst(), false));
            list = list.getRest();
        }

        List<Note> notes = new  LinkedList<>();
        notes.add(note);
        noteWriter.toWikiText(notes, out, true);
    }

    private Note toNote(final Atom atom, final boolean withValueAndProperties) {
        Note note = new Note();
        note.setId(atom.getId());
        if (withValueAndProperties) {
            note.setValue(atom.getValue());
            note.setAlias(atom.getAlias());
            note.setCreated(atom.getCreated());
            note.setPriority(atom.getPriority());
            note.setSharability(atom.getSharability());
            note.setShortcut(atom.getShortcut());
            note.setWeight(atom.getWeight());
        }
        return note;
    }

    private <E extends Exception> void timeIt(final String description,
                                              final RunnableWithException<E> action) throws E {
        long before = System.currentTimeMillis();
        action.run();
        long after = System.currentTimeMillis();

        logger.info(description + " in " + (after-before) + " ms");
    }

    private interface RunnableWithException<E extends Exception> {
        void run() throws E;
    }
}
