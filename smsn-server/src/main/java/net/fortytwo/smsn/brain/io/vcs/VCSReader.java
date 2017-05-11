package net.fortytwo.smsn.brain.io.vcs;

import net.fortytwo.smsn.SemanticSynchrony;
import net.fortytwo.smsn.brain.io.BrainReader;
import net.fortytwo.smsn.brain.io.Format;
import net.fortytwo.smsn.brain.io.wiki.WikiParser;
import net.fortytwo.smsn.brain.model.Note;
import net.fortytwo.smsn.brain.model.TopicGraph;
import net.fortytwo.smsn.brain.model.entities.Atom;
import net.fortytwo.smsn.brain.model.entities.EntityList;
import net.fortytwo.smsn.config.DataSource;
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
        for (DataSource source : SemanticSynchrony.getConfiguration().getSources()) {
            readDataSource(source, context);
        }
    }

    @Override
    public List<Format> getFormats() {
        return Arrays.asList(VCSFormat.getInstance());
    }

    private void readDataSource(final DataSource dataSource, final Context context) throws IOException {
        String location = dataSource.getLocation();
        Preconditions.checkNotNull(location);
        File dir = new File(location);
        assertDirectoryExists(dir);

        Helper helper = new Helper(context);

        File[] files = dir.listFiles();
        if (null != files) {
            for (File file : files) {
                try {
                    if (VCSFormat.isAtomFile(file)) {
                        readAtomFile(file, helper, dataSource);
                    }
                } catch (IOException e) {
                    throw new IOException("failed to load file " + file.getAbsolutePath(), e);
                }
            }
        }
    }

    private void readAtomFile(final File file, final Helper helper, final DataSource source) throws IOException {
        Note rootNote;
        try (InputStream in = new FileInputStream(file)) {
            rootNote = reader.parse(in);
            for (Note note : rootNote.getChildren()) {
                note.setSource(source.getName());
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
            updateProperty(atom, note, Note::getAlias, Atom::setAlias);
            updateProperty(atom, note, Note::getCreated, Atom::setCreated);
            updateProperty(atom, note, Note::getPage, Atom::setText);
            updateProperty(atom, note, Note::getPriority, Atom::setPriority);
            updateProperty(atom, note, Note::getSource, Atom::setSource);
            updateProperty(atom, note, Note::getShortcut, Atom::setShortcut);
            updateProperty(atom, note, Note::getSource, Atom::setSource);
            updateProperty(atom, note, Note::getTitle, Atom::setTitle);
            updateProperty(atom, note, Note::getWeight, Atom::setWeight);
        }

        private void updateAtomChildren() {
            Optional<EntityList<Atom>> newChildren = createAtomList();
            if (newChildren.isPresent()) {
                atom.setChildren(newChildren.get());
            }
        }

        private <T> void updateProperty(final Atom atom,
                                        final Note note,
                                        final Function<Note, T> noteGetter,
                                        final BiConsumer<Atom, T> atomSetter) {
            T value = noteGetter.apply(note);
            if (null != value) {
                atomSetter.accept(atom, value);
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
            Optional<Atom> opt = graph.getAtomById(id);
            Atom referenced;
            if (opt.isPresent()) {
                referenced = opt.get();
            } else {
                referenced = graph.createAtom(id);
            }
            return referenced;
        }
    }
}
