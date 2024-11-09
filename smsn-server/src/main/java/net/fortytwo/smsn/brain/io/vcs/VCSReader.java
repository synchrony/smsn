package net.fortytwo.smsn.brain.io.vcs;

import net.fortytwo.smsn.SemanticSynchrony;
import net.fortytwo.smsn.brain.AtomId;
import net.fortytwo.smsn.brain.io.NoteReader;
import net.fortytwo.smsn.brain.io.Format;
import net.fortytwo.smsn.brain.io.wiki.WikiParser;
import net.fortytwo.smsn.brain.model.TopicGraph;
import net.fortytwo.smsn.brain.model.entities.Note;
import net.fortytwo.smsn.brain.model.entities.Link;
import net.fortytwo.smsn.brain.model.entities.ListNode;
import net.fortytwo.smsn.brain.model.entities.Page;
import net.fortytwo.smsn.brain.model.entities.TreeNode;
import net.fortytwo.smsn.brain.query.TreeViews;
import net.fortytwo.smsn.config.DataSource;

import java.io.File;
import java.io.FileInputStream;
import java.io.IOException;
import java.io.InputStream;
import java.util.Collections;
import java.util.List;
import java.util.Optional;
import java.util.function.BiConsumer;
import java.util.function.Function;

public class VCSReader extends NoteReader {
    private final FilePerNoteFormat format = VCSWriter.FORMAT;

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
        return Collections.singletonList(format);
    }

    private void readDataSource(final DataSource dataSource, final Context context) throws IOException {
        String location = dataSource.getLocation();
        File dir = new File(location);
        assertDirectoryExists(dir);

        Helper helper = new Helper(context);

        File[] files = dir.listFiles();
        if (null != files) {
            for (File file : files) {
                try {
                    if (format.isMatchingFile(file)) {
                        readPage(file, helper, dataSource);
                    }
                } catch (IOException e) {
                    throw new IOException("failed to load file " + file.getAbsolutePath(), e);
                }
            }
        }
    }

    private AtomId idFromFileName(final File file) {
        String fileName = file.getName();
        return new AtomId(fileName.substring(0, fileName.indexOf(".")));
    }

    private void readPage(final File file, final Helper helper, final DataSource source) throws IOException {
        Page page;

        try (InputStream in = new FileInputStream(file)) {
            try {
                page = reader.parse(in);
            } catch (IOException e) {
                throw new IOException("parse error in VCS file " + file, e);
            }
            AtomId rootId = idFromFileName(file);
            Note root = helper.resolveNoteReference(rootId);

            for (TreeNode<Link> note : TreeViews.getChildrenAsList(page.getContent())) {
                AtomId id = TreeViews.getId(note);
//                Preconditions.checkNotNull(id);
            }
            page.setSource(source.getName());
            helper.setNote(root);
            helper.setPage(page);
            helper.updateNote();

            checkAndCommit(helper.context.getTopicGraph());
        }
    }

    private class Helper {
        private final Context context;
        private Note note;
        private Page page;

        private Helper(Context context) {
            this.context = context;
        }

        public void setNote(Note note) {
            this.note = note;
        }

        public void setPage(Page page) {
            this.page = page;
        }

        private void updateNote() {
            updateNoteProperties();
            updateNoteChildren();
        }

        private void updateNoteProperties() {
            updatePageProperty(note, page, p -> page.getCreated(), Note::setCreated);
            updatePageProperty(note, page, p -> page.getText(), Note::setText);
            updatePageProperty(note, page, p -> page.getAlias(), Note::setAlias);
            updatePageProperty(note, page, p -> page.getPriority(), Note::setPriority);
            updatePageProperty(note, page, p -> page.getSource(), Note::setSource);
            updatePageProperty(note, page, p -> page.getShortcut(), Note::setShortcut);
            updatePageProperty(note, page, p -> page.getText(), Note::setText);
            updatePageProperty(note, page, p -> page.getWeight(), Note::setWeight);

            TreeNode<Link> tree = page.getContent();
            updateTreeProperty(note, tree, TreeViews::getTitle, Note::setTitle);
        }

        private void updateNoteChildren() {
            Optional<ListNode<Note>> newChildren = createNoteList();
            if (newChildren.isPresent()) {
                note.setChildren(newChildren.get());
            }
        }

        private <T> void updatePageProperty(final Note note,
                                            final Page page,
                                            final Function<Page, T> noteGetter,
                                            final BiConsumer<Note, T> noteSetter) {
            T value = noteGetter.apply(page);
            if (null != value) {
                noteSetter.accept(note, value);
            }
        }

        private <T> void updateTreeProperty(final Note note,
                                            final TreeNode<Link> node,
                                            final Function<TreeNode<Link>, T> noteGetter,
                                            final BiConsumer<Note, T> noteSetter) {
            T value = noteGetter.apply(node);
            if (null != value) {
                noteSetter.accept(note, value);
            }
        }

        private Optional<ListNode<Note>> createNoteList() {
            if (0 == TreeViews.countChildren(page.getContent())) return Optional.empty();

            Note[] notes = new Note[TreeViews.countChildren(page.getContent())];
            int i = 0;
            for (TreeNode<Link> child : TreeViews.getChildrenAsList(page.getContent())) {
                notes[i++] = resolveNoteReference(TreeViews.getId(child));
            }
            return Optional.of(context.getTopicGraph().createListOfNotes(notes));
        }

        private Note resolveNoteReference(final AtomId id) {
            TopicGraph graph = context.getTopicGraph();
            Optional<Note> opt = graph.getNoteById(id);
            Note referenced;
            if (opt.isPresent()) {
                referenced = opt.get();
            } else {
                referenced = graph.createNote(id);
            }
            return referenced;
        }
    }
}
