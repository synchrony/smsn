package net.fortytwo.smsn.brain.io.vcs;

import com.google.common.base.Preconditions;
import net.fortytwo.smsn.brain.io.BrainWriter;
import net.fortytwo.smsn.brain.io.Format;
import net.fortytwo.smsn.brain.io.wiki.WikiPrinter;
import net.fortytwo.smsn.brain.model.entities.Atom;
import net.fortytwo.smsn.brain.model.TopicGraph;
import net.fortytwo.smsn.brain.model.entities.EntityList;
import net.fortytwo.smsn.brain.model.Note;

import java.io.File;
import java.io.FileOutputStream;
import java.io.IOException;
import java.io.OutputStream;
import java.util.List;

public class VCSWriter extends BrainWriter {

    private static final List<Format> formats;
    private static final WikiPrinter wikiPrinter;

    static {
        wikiPrinter = new WikiPrinter();
        wikiPrinter.setUseCanonicalFormat(true);
    }

    static {
        formats = new java.util.LinkedList();
        formats.add(VCSFormat.getInstance());
    }

    @Override
    public List<Format> getFormats() {
        return formats;
    }

    @Override
    public void doExport(Context context) throws IOException {
        File parentDir = context.getDestDirectory();
        createDirectoryIfNotExists(parentDir);

        File[] dirs = initializeDirectories(parentDir);

        timeAction("exported atoms as individual files", () -> doExport(context.getTopicGraph(), dirs));
    }

    private File[] initializeDirectories(final File parentDir) throws IOException {
        File[] dirs = VCSFormat.getDirsBySharability(parentDir);
        for (File d : dirs) {
            createDirectoryIfNotExists(d);
            timeAction("cleaned directory " + d, () -> clearDirectoryOfAtomData(d));
        }
        return dirs;
    }

    private void clearDirectoryOfAtomData(final File dir) {
        for (File file : dir.listFiles()) {
            if (VCSFormat.isAtomFile(file)) {
                if (!file.delete()) {
                    throw new IllegalStateException("failed to delete atom file " + file.getAbsolutePath());
                }
            }
        }
    }

    private void doExport(final TopicGraph graph, final File[] dirs) throws IOException {
        for (Atom a : graph.getAllAtoms()) {
            if (isAtomWithPage(a)) {
                File dir = chooseDirectoryForAtom(a, dirs);
                File atomFile = new File(dir, "a" + a.getId());
                try (OutputStream out = new FileOutputStream(atomFile)) {
                    writeAtomToStream(a, out);
                }
            }
        }
    }

    private boolean isAtomWithPage(final Atom a) {
        return a.getSharability() > 0f;
    }

    private File chooseDirectoryForAtom(final Atom a, File[] dirs) {
        Float sharability = a.getSharability();
        Preconditions.checkArgument(sharability > 0f && sharability <= 1.0f, a.getId());

        int index = (int) (sharability / 0.25f) - 1;
        return dirs[index];
    }

    private void writeAtomToStream(final Atom atom, final OutputStream out) {
        Note note = toNote(atom, true);
        EntityList<Atom> list = atom.getNotes();
        while (null != list) {
            note.getChildren().add(toNote(list.getFirst(), false));
            list = list.getRest();
        }

        wikiPrinter.print(note, out, true);
    }

    private Note toNote(final Atom atom, final boolean withValueAndProperties) {
        Note note = new Note();
        note.setId(atom.getId());

        if (withValueAndProperties) {
            note.setTitle(atom.getTitle());
            note.setPage(atom.getText());
            note.setAlias(atom.getAlias());
            note.setCreated(atom.getCreated());
            note.setPriority(atom.getPriority());
            note.setSharability(atom.getSharability());
            note.setShortcut(atom.getShortcut());
            note.setWeight(atom.getWeight());
        }
        return note;
    }

    private <E extends Exception> void timeAction(final String description,
                                                  final RunnableWithException<E> action) throws E {
        long before = System.currentTimeMillis();
        action.run();
        long after = System.currentTimeMillis();

        logger.info(description + " in " + (after - before) + " ms");
    }

    private interface RunnableWithException<E extends Exception> {
        void run() throws E;
    }
}
