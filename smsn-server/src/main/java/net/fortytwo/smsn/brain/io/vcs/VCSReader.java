package net.fortytwo.smsn.brain.io.vcs;

import net.fortytwo.smsn.brain.io.BrainReader;
import net.fortytwo.smsn.brain.io.Format;
import net.fortytwo.smsn.brain.io.wiki.WikiReader;
import net.fortytwo.smsn.brain.model.Atom;
import net.fortytwo.smsn.brain.model.AtomGraph;
import net.fortytwo.smsn.brain.model.AtomList;
import net.fortytwo.smsn.brain.model.Note;
import org.parboiled.common.Preconditions;

import java.io.File;
import java.io.FileInputStream;
import java.io.IOException;
import java.io.InputStream;
import java.util.Arrays;
import java.util.List;
import java.util.function.BiConsumer;
import java.util.function.Function;

public class VCSReader extends BrainReader {
    public enum OverwritePolicy {Preserve, Replace}

    private final WikiReader reader = new WikiReader();

    private final OverwritePolicy policy = OverwritePolicy.Replace;

    @Override
    protected void importInternal(Context context) throws IOException {
        File[] dirs = VCSFormat.getDirsBySharability(context.getSourceDirectory());

        for (File d : dirs) assertDirectoryExists(d);

        for (File d : dirs) {
            readDirectory(d, context);
        }
    }

    @Override
    public List<Format> getFormats() {
        return Arrays.asList(VCSFormat.getInstance());
    }

    private void readDirectory(final File dir, final Context context)
            throws IOException {
        Helper helper = new Helper(context);

        for (File file : dir.listFiles()) {
            if (VCSFormat.isAtomFile(file)) {
                readAtomFile(file, helper);
            }
        }
    }

    private void readAtomFile(final File file, final Helper helper) throws IOException {

        Note rootNote;
        try (InputStream in = new FileInputStream(file)) {
            rootNote = reader.parse(in);
            for (Note note : rootNote.getChildren()) {
                String id = note.getId();
                Preconditions.checkNotNull(id);

                Atom atom = helper.resolveAtomReference(id);
                helper.setNote(note);
                helper.setAtom(atom);
                helper.updateAtom();
                //System.out.println("root\t" + rootNote.getId() + "\t" + rootNote.getValue());
            }
        }
    }

    private class Helper {
        private final Context context;
        private final boolean doReplace;
        private Atom atom;
        private Note note;

        private Helper(Context context) {
            this.context = context;
            this.doReplace = policy.equals(OverwritePolicy.Replace);
        }

        public void setAtom(Atom atom) {
            this.atom = atom;
        }

        public void setNote(Note note) {
            this.note = note;
        }

        private void updateAtom() {
            updateAtomProperties();
            updateAtomChildren();
        }

        private void updateAtomProperties() {
            updateProperty(atom, note, Atom::getAlias, Note::getAlias, Atom::setAlias);
            updateProperty(atom, note, Atom::getCreated, Note::getCreated, Atom::setCreated);
            updateProperty(atom, note, Atom::getPriority, Note::getPriority, Atom::setPriority);
            updateProperty(atom, note, Atom::getSharability, Note::getSharability, Atom::setSharability);
            updateProperty(atom, note, Atom::getShortcut, Note::getShortcut, Atom::setShortcut);
            updateProperty(atom, note, Atom::getValue, Note::getValue, Atom::setValue);
            updateProperty(atom, note, Atom::getWeight, Note::getWeight, Atom::setWeight);
        }

        private void updateAtomChildren() {
            if (doReplace || null == atom.getNotes()) {
                removeAllChildren();
                atom.setNotes(createAtomList());
            }
        }

        private <T> void updateProperty(final Atom atom,
                                        final Note note,
                                        final Function<Atom, T> atomGetter,
                                        final Function<Note, T> noteGetter,
                                        final BiConsumer<Atom, T> atomSetter) {
            T value = noteGetter.apply(note);
            if (null != value && (doReplace || null == atomGetter.apply(atom))) {
                atomSetter.accept(atom, value);
            }
        }

        private void removeAllChildren() {
            AtomList list = atom.getNotes();
            if (null != list) {
                while (null != list) {
                    AtomList rest = list.getRest();
                    list.setFirst(null);
                    list.setRest(null);
                    list = rest;
                }

                atom.setNotes(null);
            }
        }

        private AtomList createAtomList() {
            AtomList list = null;
            List<Note> children = note.getChildren();
            if (null != children) {
                for (int i = children.size() - 1; i >= 0; i--) {
                    Note child = children.get(i);
                    AtomList cur = context.getAtomGraph().createAtomList((String) null);
                    cur.setFirst(resolveAtomReference(child.getId()));
                    cur.setRest(list);
                    list = cur;
                }
            }
            return list;
        }

        private Atom resolveAtomReference(final String id) {
            AtomGraph graph = context.getAtomGraph();
            Atom atom = graph.getAtomById(id);
            if (null == atom) {
                atom = graph.createAtom(id);
            }
            return atom;
        }
    }
}
