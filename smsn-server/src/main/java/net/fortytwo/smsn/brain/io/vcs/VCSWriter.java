package net.fortytwo.smsn.brain.io.vcs;

import com.google.common.base.Preconditions;
import net.fortytwo.smsn.brain.io.BrainWriter;
import net.fortytwo.smsn.brain.io.Format;
import net.fortytwo.smsn.brain.io.wiki.WikiPrinter;
import net.fortytwo.smsn.brain.model.Role;
import net.fortytwo.smsn.brain.model.TopicGraph;
import net.fortytwo.smsn.brain.model.dto.LinkDTO;
import net.fortytwo.smsn.brain.model.dto.ListNodeDTO;
import net.fortytwo.smsn.brain.model.dto.PageDTO;
import net.fortytwo.smsn.brain.model.dto.TopicDTO;
import net.fortytwo.smsn.brain.model.dto.TreeNodeDTO;
import net.fortytwo.smsn.brain.model.entities.Atom;
import net.fortytwo.smsn.brain.model.entities.Link;
import net.fortytwo.smsn.brain.model.entities.ListNode;
import net.fortytwo.smsn.brain.model.entities.Page;
import net.fortytwo.smsn.brain.model.entities.Topic;
import net.fortytwo.smsn.brain.model.entities.TreeNode;
import net.fortytwo.smsn.brain.query.TreeViews;

import java.io.File;
import java.io.FileOutputStream;
import java.io.IOException;
import java.io.OutputStream;
import java.util.LinkedList;
import java.util.List;
import java.util.Map;

public class VCSWriter extends BrainWriter {

    private static final List<Format> formats;

    static {
        formats = new LinkedList<>();
        formats.add(VCSFormat.getInstance());
    }

    @Override
    public List<Format> getFormats() {
        return formats;
    }

    @Override
    public void doExport(Context context) throws IOException {
        Map<String, File> dirs = initializeDirectories();

        timeAction("exported atoms as individual files", () -> doExport(context.getTopicGraph(), dirs));
    }

    private Map<String, File> initializeDirectories() throws IOException {
        Map<String, File> dirs = VCSFormat.getDirsBySource();
        for (File d : dirs.values()) {
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

    private void doExport(final TopicGraph graph, final Map<String, File> dirs) throws IOException {
        for (Atom a : graph.getAllAtoms()) {
            if (isAtomWithPage(a)) {
                File dir = chooseDirectoryForAtom(a, dirs);
                File atomFile = new File(dir, fileNameForAtom(a));
                try (OutputStream out = new FileOutputStream(atomFile)) {
                    writeAtomToStream(a, out);
                }
            }
        }
    }

    private String fileNameForAtom(final Atom a) {
        return a.getId();
    }

    private boolean isAtomWithPage(final Atom a) {
        return null != a.getSource();
    }

    private File chooseDirectoryForAtom(final Atom a, Map<String, File> dirs) {
        String source = a.getSource();
        Preconditions.checkNotNull(source);
        File dir = dirs.get(source);
        Preconditions.checkNotNull(dir);
        return dir;
    }

    private void writeAtomToStream(final Atom atom, final OutputStream out) {
        TreeNode<Link> tree = new TreeNodeDTO<>();
        Topic topic = new TopicDTO();
        topic.setId(atom.getId());
        Link link = new LinkDTO();
        link.setTarget(topic);
        link.setLabel(atom.getTitle());
        link.setRole(Role.Noun);
        tree.setValue(link);

        ListNode<Atom> cur = atom.getChildren();
        while (null != cur) {
            tree.addChild(toTree(cur.getFirst()));
            cur = cur.getRest();
        }

        Page page = PageDTO.createTransitional();
        page.setContent(tree);
        page.setCreated(atom.getCreated());
        page.setShortcut(atom.getShortcut());
        page.setText(atom.getText());
        page.setAlias(atom.getAlias());
        page.setPriority(atom.getPriority());
        page.setWeight(atom.getWeight());
        new WikiPrinter(out).print(page);
    }

    private TreeNode<Link> toTree(final Atom atom) {
        TreeNode<Link> tree = TreeNodeDTO.createEmptyNode();
        TreeViews.setId(tree, atom.getId());
        return tree;
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
