package net.fortytwo.smsn.brain.io.vcs;

import net.fortytwo.smsn.brain.io.BrainReader;
import net.fortytwo.smsn.brain.io.Format;
import net.fortytwo.smsn.brain.io.wiki.WikiParser;
import net.fortytwo.smsn.brain.model.Note;
import net.fortytwo.smsn.brain.model.TopicGraph;
import net.fortytwo.smsn.brain.model.entities.Atom;
import net.fortytwo.smsn.brain.model.entities.EntityList;
import org.parboiled.common.Preconditions;

import java.io.File;
import java.io.FileInputStream;
import java.io.IOException;
import java.io.InputStream;
import java.util.Arrays;
import java.util.List;
import java.util.Optional;
import java.util.function.BiConsumer;
import java.util.function.Function;

public class VCSReader extends BrainReader {

    private final WikiParser reader;

    public VCSReader() {
        reader = new WikiParser();
        reader.setUseCanonicalFormat(true);
    }

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

        File[] files = dir.listFiles();
        if (null != files) {
            for (File file : files) {
                try {
                    if (VCSFormat.isAtomFile(file)) {
                        readAtomFile(file, helper);
                    }
                } catch (IOException e) {
                    throw new IOException("failed to load file " + file.getAbsolutePath(), e);
                }
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

                checkAndCommit(helper.context.getTopicGraph());
            }
        }
    }

    private class Helper {
        private final Context context;
        private Atom atom;
        private Note note;

        private Helper(Context context) {
            this.context = context;
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
            updateProperty(atom, note, Atom::getText, Note::getPage, Atom::setText);
            updateProperty(atom, note, Atom::getPriority, Note::getPriority, Atom::setPriority);
            updateProperty(atom, note, Atom::getSharability, Note::getSharability, Atom::setSharability);
            updateProperty(atom, note, Atom::getShortcut, Note::getShortcut, Atom::setShortcut);
            updateProperty(atom, note, Atom::getTitle, Note::getTitle, Atom::setTitle);
            updateProperty(atom, note, Atom::getWeight, Note::getWeight, Atom::setWeight);
        }

        private void updateAtomChildren() {
            removeAllChildren();
            Optional<EntityList<Atom>> newChildren = createAtomList();
            if (newChildren.isPresent()) {
                atom.setNotes(newChildren.get());
            }
        }

        private <T> void updateProperty(final Atom atom,
                                        final Note note,
                                        final Function<Atom, T> atomGetter,
                                        final Function<Note, T> noteGetter,
                                        final BiConsumer<Atom, T> atomSetter) {
            T value = noteGetter.apply(note);
            if (null != value) {
                atomSetter.accept(atom, value);
            }
        }

        private void removeAllChildren() {
            EntityList<Atom> list = atom.getNotes();
            if (null != list) {
                while (null != list) {
                    EntityList<Atom> rest = list.getRest();
                    list.destroy();
                    list = rest;
                }

                atom.setNotes(null);
            }
        }

        private Optional<EntityList<Atom>> createAtomList() {
            if (0 == note.getChildren().size()) return Optional.empty();

            Atom[] atoms = new Atom[note.getChildren().size()];
            int i = 0;
            for (Note child : note.getChildren()) {
                atoms[i++] = resolveAtomReference(child.getId());
            }
            return Optional.of(context.getTopicGraph().createListOfAtoms(atoms));
        }

        private Atom resolveAtomReference(final String id) {
            TopicGraph graph = context.getTopicGraph();
            Atom atom = graph.getAtomById(id);
            if (null == atom) {
                atom = graph.createAtom(id);
            }
            return atom;
        }
    }
}
