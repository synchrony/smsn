package net.fortytwo.smsn.brain.io.vcs;

import com.google.common.base.Preconditions;
import net.fortytwo.smsn.brain.io.NoteWriter;
import net.fortytwo.smsn.brain.io.Format;
import net.fortytwo.smsn.brain.io.wiki.WikiPrinter;
import net.fortytwo.smsn.brain.model.TopicGraph;
import net.fortytwo.smsn.brain.model.dto.LinkDTO;
import net.fortytwo.smsn.brain.model.dto.PageDTO;
import net.fortytwo.smsn.brain.model.dto.TopicDTO;
import net.fortytwo.smsn.brain.model.dto.TreeNodeDTO;
import net.fortytwo.smsn.brain.model.entities.Note;
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

public class VCSWriter extends NoteWriter {

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
    public void doWrite(Context context) throws IOException {
        Map<String, File> dirs = initializeDirectories();

        timeAction("exported notes as individual files", () -> doExport(context.getTopicGraph(), dirs));
    }

    private Map<String, File> initializeDirectories() throws IOException {
        Map<String, File> dirs = VCSFormat.getDirsBySource();
        for (File d : dirs.values()) {
            createDirectoryIfNotExists(d);
            timeAction("cleaned directory " + d, () -> clearDirectoryOfSmSnData(d));
        }
        return dirs;
    }

    private void clearDirectoryOfSmSnData(final File dir) {
        for (File file : dir.listFiles()) {
            if (VCSFormat.isSmSnFile(file)) {
                if (!file.delete()) {
                    throw new IllegalStateException("failed to delete SmSn file " + file.getAbsolutePath());
                }
            }
        }
    }

    private void doExport(final TopicGraph graph, final Map<String, File> dirs) throws IOException {
        for (Note a : graph.getAllNotes()) {
            if (isNoteWithPage(a)) {
                File dir = chooseDirectoryForNote(a, dirs);
                File pageFile = new File(dir, fileNameForNote(a));
                try (OutputStream out = new FileOutputStream(pageFile)) {
                    writeNoteToStream(a, out);
                }
            }
        }
    }

    private String fileNameForNote(final Note a) {
        // TODO
        Topic topic = new TopicDTO();
        topic.setId(Note.getId(a));
        return VCSFormat.fileNameForTopic(topic);
    }

    private boolean isNoteWithPage(final Note a) {
        return null != Note.getSource(a);
    }

    private File chooseDirectoryForNote(final Note a, Map<String, File> dirs) {
        String source = Note.getSource(a);
        Preconditions.checkNotNull(source);
        File dir = dirs.get(source);
        Preconditions.checkNotNull(dir);
        return dir;
    }

    private void writeNoteToStream(final Note note, final OutputStream out) {
        TreeNode<Link> tree = new TreeNodeDTO<>();
        Topic topic = new TopicDTO();
        topic.setId(Note.getId(note));
        Link link = new LinkDTO();
        link.setTarget(topic);
        link.setLabel(Note.getTitle(note));
        tree.setValue(link);

        ListNode<Note> cur = note.getChildren();
        while (null != cur) {
            tree.addChild(toTree(cur.getFirst()));
            cur = cur.getRest();
        }

        Page page = PageDTO.createTransitional();
        page.setContent(tree);
        page.setCreated(Note.getCreated(note));
        page.setShortcut(Note.getShortcut(note));
        page.setText(Note.getText(note));
        page.setAlias(Note.getAlias(note));
        page.setPriority(Note.getPriority(note));
        page.setWeight(Note.getWeight(note));
        new WikiPrinter(out).print(page);
    }

    private TreeNode<Link> toTree(final Note note) {
        TreeNode<Link> tree = TreeNodeDTO.createEmptyNode();
        TreeViews.setId(tree, Note.getId(note));
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
