package net.fortytwo.smsn.brain.io.vcs;

import net.fortytwo.smsn.SemanticSynchrony;
import net.fortytwo.smsn.brain.io.NoteReader;
import net.fortytwo.smsn.brain.io.Format;
import net.fortytwo.smsn.brain.io.wiki.WikiParser;
import net.fortytwo.smsn.brain.model.Tag;
import net.fortytwo.smsn.brain.model.TopicGraph;
import net.fortytwo.smsn.brain.model.entities.Note;
import net.fortytwo.smsn.brain.model.entities.Link;
import net.fortytwo.smsn.brain.model.entities.ListNode;
import net.fortytwo.smsn.brain.model.entities.Page;
import net.fortytwo.smsn.brain.model.entities.TreeNode;
import net.fortytwo.smsn.brain.query.TreeViews;
import net.fortytwo.smsn.config.DataSource;
import org.parboiled.common.Preconditions;

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
                    if (VCSFormat.isSmSnFile(file)) {
                        readPage(file, helper, dataSource);
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

    private void readPage(final File file, final Helper helper, final DataSource source) throws IOException {
        Page page;
        try (InputStream in = new FileInputStream(file)) {
            page = reader.parse(in);
            String rootId = idFromFileName(file);
            Note root = helper.resolveAtomReference(rootId);

            for (TreeNode<Link> note : TreeViews.getChildrenAsList(page.getContent())) {
                String id = TreeViews.getId(note);
                Preconditions.checkNotNull(id);
            }
            page.setSource(source.getName());
            helper.setAtom(root);
            helper.setPage(page);
            helper.updateAtom();

            checkAndCommit(helper.context.getTopicGraph());
        }
    }

    private class Helper {
        private final Context context;
        private Note atom;
        private Page page;

        private Helper(Context context) {
            this.context = context;
        }

        public void setAtom(Note atom) {
            this.atom = atom;
        }

        public void setPage(Page page) {
            this.page = page;
        }

        private void updateAtom() {
            updateAtomProperties();
            updateAtomChildren();
        }

        private void updateAtomProperties() {
            updatePageProperty(atom, page, p -> page.getCreated(), Note::setCreated);
            updatePageProperty(atom, page, p -> page.getText(), Note::setText);
            updatePageProperty(atom, page, p -> page.getAlias(), Note::setAlias);
            updatePageProperty(atom, page, p -> page.getPriority(), Note::setPriority);
            updatePageProperty(atom, page, p -> page.getSource(), Note::setSource);
            updatePageProperty(atom, page, p -> page.getShortcut(), Note::setShortcut);
            updatePageProperty(atom, page, p -> page.getText(), Note::setText);
            updatePageProperty(atom, page, p -> page.getWeight(), Note::setWeight);

            TreeNode<Link> tree = page.getContent();
            updateTreeProperty(atom, tree, TreeViews::getTitle, Note::setTitle);
        }

        private void updateAtomChildren() {
            Optional<ListNode<Note>> newChildren = createAtomList();
            if (newChildren.isPresent()) {
                atom.setChildren(newChildren.get());
            }
        }

        private <T> void updatePageProperty(final Note atom,
                                            final Page page,
                                            final Function<Page, T> noteGetter,
                                            final BiConsumer<Note, T> atomSetter) {
            T value = noteGetter.apply(page);
            if (null != value) {
                atomSetter.accept(atom, value);
            }
        }

        private <T> void updateTreeProperty(final Note atom,
                                            final TreeNode<Link> note,
                                            final Function<TreeNode<Link>, T> noteGetter,
                                            final BiConsumer<Note, T> atomSetter) {
            T value = noteGetter.apply(note);
            if (null != value) {
                atomSetter.accept(atom, value);
            }
        }

        private Optional<ListNode<Note>> createAtomList() {
            if (0 == TreeViews.countChildren(page.getContent())) return Optional.empty();

            Note[] atoms = new Note[TreeViews.countChildren(page.getContent())];
            int i = 0;
            for (TreeNode<Link> child : TreeViews.getChildrenAsList(page.getContent())) {
                atoms[i++] = resolveAtomReference(TreeViews.getId(child));
            }
            return Optional.of(context.getTopicGraph().createListOfNotes(atoms));
        }

        private Note resolveAtomReference(final String id) {
            TopicGraph graph = context.getTopicGraph();
            Optional<Note> opt = graph.getNotesById(id);
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
