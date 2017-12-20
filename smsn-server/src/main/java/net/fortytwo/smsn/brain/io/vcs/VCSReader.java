package net.fortytwo.smsn.brain.io.vcs;

import net.fortytwo.smsn.SemanticSynchrony;
import net.fortytwo.smsn.brain.error.InvalidGraphException;
import net.fortytwo.smsn.brain.io.Format;
import net.fortytwo.smsn.brain.io.NoteReader;
import net.fortytwo.smsn.brain.io.wiki.WikiParser;
import net.fortytwo.smsn.brain.model.Filter;
import net.fortytwo.smsn.brain.model.entities.Note;
import net.fortytwo.smsn.brain.model.entities.Topic;
import net.fortytwo.smsn.brain.query.ViewStyle;
import net.fortytwo.smsn.config.DataSource;
import org.parboiled.common.Preconditions;

import java.io.File;
import java.io.FileInputStream;
import java.io.IOException;
import java.io.InputStream;
import java.util.Collections;
import java.util.List;
import java.util.Optional;
import java.util.function.Consumer;

public class VCSReader extends NoteReader {

    private final WikiParser reader;

    public VCSReader() {
        reader = new WikiParser();
    }

    @Override
    protected void importInternal(Context context) throws IOException {
        for (DataSource source : SemanticSynchrony.getConfiguration().getSources()) {
            readDataSource(source, context);
        }
    }

    @Override
    public List<Format> getFormats() {
        return Collections.singletonList(VCSFormat.getInstance());
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
                    if (VCSFormat.isDataFile(file)) {
                        readDataFile(file, helper, dataSource);
                    }
                } catch (IOException e) {
                    throw new IOException("failed to load file " + file.getAbsolutePath(), e);
                }
            }
        }
    }

    private String idFromFileName(final File file) {
        String fileName = file.getName();
        return fileName.substring(0, fileName.indexOf("."));
    }

    private void readDataFile(final File file, final Helper helper, final DataSource source) throws IOException {
        forDataFile(file, source, sourceNote -> {
            String rootId = idFromFileName(file);
            Note destNote = helper.getOrCreateTopicRootForSource(rootId, source);
            helper.setFromNote(destNote);
            helper.setToNote(sourceNote);
            helper.updateNote();

            checkAndCommit(helper.context.getTopicGraph());
        });
    }

    private void forDataFile(final File file, final DataSource source, final Consumer<Note> consumer)
            throws IOException {
        try (InputStream in = new FileInputStream(file)) {
            Note note;
            try {
                note = reader.parse(in);
            } catch (IOException e) {
                throw new IOException("parse error in VCS file " + file, e);
            }

            Note.forAllChildren(note, this::checkHasTopic);
            note.setSource(source.getName());

            consumer.accept(note);
        }
    }

    private void checkHasTopic(final Note note) {
        Topic topic = note.getTopic();
        Preconditions.checkNotNull(topic);
    }

    private class Helper {
        private final Context context;
        private Note fromNote;
        private Note toNote;

        private Helper(Context context) {
            this.context = context;
        }

        public void setFromNote(Note fromNote) {
            this.fromNote = fromNote;
        }

        public void setToNote(Note toNote) {
            this.toNote = toNote;
        }

        private void updateNote() {
            updateNoteProperties();
            updateChildren();
        }

        private void updateNoteProperties() {
            Note.copyProperties(fromNote, toNote);
        }

        private void updateChildren() {
            context.getModel().view()
                    .root(toNote).height(1).filter(Filter.noFilter()).style(ViewStyle.Basic.Forward.getStyle())
                    .put(fromNote);
        }

        private Note getOrCreateTopicRootForSource(final String topicId, final DataSource source) {
            Mutable<Note> root = new Mutable<>();
            Optional<Topic> opt = context.getTopicGraph().getTopicById(topicId);
            Topic topic;
            if (opt.isPresent()) {
                topic = opt.get();
                forEachTopicRoot(topic, note -> {
                    if (note.getSource().equals(source.getName())) {
                        if (null != root.getValue()) {
                            throw new InvalidGraphException(
                                    "multiple roots for topic " + topic.getId() + " in '" + source.getName() + "'");
                        }
                        root.setValue(note);
                    }
                });
            } else {
                topic = context.getTopicGraph().createTopic(topicId);
            }

            if (null == root.getValue()) {
                return context.getTopicGraph().createNote(topic, null, null);
            } else {
                return root.getValue();
            }
        }

        private void forEachTopicRoot(final Topic topic, final Consumer<Note> consumer) {
            topic.forEachNote(note -> {
                if (Note.isRoot(note)) {
                    consumer.accept(note);
                }
            });
        }
    }

    private static class Mutable<T> {
        private T value;

        public T getValue() {
            return value;
        }

        public void setValue(T value) {
            this.value = value;
        }
    }
}
