package net.fortytwo.smsn.brain.io.vcs;

import net.fortytwo.smsn.SemanticSynchrony;
import net.fortytwo.smsn.brain.io.BrainReader;
import net.fortytwo.smsn.brain.io.Format;
import net.fortytwo.smsn.brain.io.wiki.WikiParser;
import net.fortytwo.smsn.brain.model.TopicGraph;
import net.fortytwo.smsn.brain.model.entities.Atom;
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

public class VCSReader extends BrainReader {

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
                    if (VCSFormat.isAtomFile(file)) {
                        readAtomFile(file, helper, dataSource);
                    }
                } catch (IOException e) {
                    throw new IOException("failed to load file " + file.getAbsolutePath(), e);
                }
            }
        }
    }

    private String idFromFileName(final File file) {
        return file.getName();
    }

    private void readAtomFile(final File file, final Helper helper, final DataSource source) throws IOException {
        Page page;
        try (InputStream in = new FileInputStream(file)) {
            page = reader.parse(in);
            String rootId = idFromFileName(file);
            Atom root = helper.resolveAtomReference(rootId);

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
        private Atom atom;
        private Page page;

        private Helper(Context context) {
            this.context = context;
        }

        public void setAtom(Atom atom) {
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
            updatePageProperty(atom, page, p -> page.getCreated(), Atom::setCreated);
            updatePageProperty(atom, page, p -> page.getText(), Atom::setText);
            updatePageProperty(atom, page, p -> page.getAlias(), Atom::setAlias);
            updatePageProperty(atom, page, p -> page.getPriority(), Atom::setPriority);
            updatePageProperty(atom, page, p -> page.getSource(), Atom::setSource);
            updatePageProperty(atom, page, p -> page.getShortcut(), Atom::setShortcut);
            updatePageProperty(atom, page, p -> page.getText(), Atom::setText);
            updatePageProperty(atom, page, p -> page.getWeight(), Atom::setWeight);

            TreeNode<Link> tree = page.getContent();
            updateTreeProperty(atom, tree, TreeViews::getTitle, Atom::setTitle);
        }

        private void updateAtomChildren() {
            Optional<ListNode<Atom>> newChildren = createAtomList();
            if (newChildren.isPresent()) {
                atom.setChildren(newChildren.get());
            }
        }

        private <T> void updatePageProperty(final Atom atom,
                                            final Page page,
                                            final Function<Page, T> noteGetter,
                                            final BiConsumer<Atom, T> atomSetter) {
            T value = noteGetter.apply(page);
            if (null != value) {
                atomSetter.accept(atom, value);
            }
        }

        private <T> void updateTreeProperty(final Atom atom,
                                            final TreeNode<Link> note,
                                            final Function<TreeNode<Link>, T> noteGetter,
                                            final BiConsumer<Atom, T> atomSetter) {
            T value = noteGetter.apply(note);
            if (null != value) {
                atomSetter.accept(atom, value);
            }
        }

        private Optional<ListNode<Atom>> createAtomList() {
            if (0 == TreeViews.countChildren(page.getContent())) return Optional.empty();

            Atom[] atoms = new Atom[TreeViews.countChildren(page.getContent())];
            int i = 0;
            for (TreeNode<Link> child : TreeViews.getChildrenAsList(page.getContent())) {
                atoms[i++] = resolveAtomReference(TreeViews.getId(child));
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
