package net.fortytwo.smsn.brain.io.vcs;

import com.google.common.base.Preconditions;
import net.fortytwo.smsn.brain.io.Format;
import net.fortytwo.smsn.brain.io.NoteWriter;
import net.fortytwo.smsn.brain.model.TopicGraph;
import net.fortytwo.smsn.brain.model.dto.LinkDTO;
import net.fortytwo.smsn.brain.model.dto.ListNodeDTO;
import net.fortytwo.smsn.brain.model.dto.PageDTO;
import net.fortytwo.smsn.brain.model.dto.TopicDTO;
import net.fortytwo.smsn.brain.model.dto.TreeNodeDTO;
import net.fortytwo.smsn.brain.model.entities.Link;
import net.fortytwo.smsn.brain.model.entities.ListNode;
import net.fortytwo.smsn.brain.model.entities.Note;
import net.fortytwo.smsn.brain.model.entities.Page;
import net.fortytwo.smsn.brain.model.entities.Topic;
import net.fortytwo.smsn.brain.model.entities.TreeNode;
import net.fortytwo.smsn.brain.query.TreeViews;

import java.io.File;
import java.io.FileOutputStream;
import java.io.IOException;
import java.io.OutputStream;
import java.util.Collections;
import java.util.List;
import java.util.Map;
import java.util.function.BiConsumer;

public abstract class FilePerNoteWriter extends NoteWriter {

    public final FilePerNoteFormat format;
    private final BiConsumer<Note, OutputStream> printer;

    public FilePerNoteWriter(FilePerNoteFormat format, BiConsumer<Note, OutputStream> printer) {
        this.format = format;
        this.printer = printer;
    }

    @Override
    public List<Format> getFormats() {
        return Collections.singletonList(format);
    }

    @Override
    public void doWrite(Context context) throws IOException {
        Map<String, File> dirs = initializeDirectories(true);

        timeAction("exported notes as individual files", () -> doExport(context.getTopicGraph(), dirs));
    }

    private Map<String, File> initializeDirectories(boolean removeOldFiles) throws IOException {
        Map<String, File> dirs = FilePerNoteFormat.directoriesBySource();
        for (File d : dirs.values()) {
            createDirectoryIfNotExists(d);
            if (removeOldFiles) {
                timeAction("cleaned directory " + d, () -> clearDirectoryOfSmSnData(d));
            }
        }
        return dirs;
    }

    private void clearDirectoryOfSmSnData(final File dir) {
        for (File file : dir.listFiles()) {
            if (format.isMatchingFile(file)) {
                if (!file.delete()) {
                    throw new IllegalStateException("failed to delete SmSn file " + file.getAbsolutePath());
                }
            }
        }
    }

    private void doExport(final TopicGraph graph, final Map<String, File> dirs) throws IOException {
        for (Note a : graph.getAllNotes()) {
            if (isMaterialized(a)) {
                File dir = chooseDirectoryForNote(a, dirs);
                File pageFile = new File(dir, fileNameFor(a));
                try (OutputStream out = new FileOutputStream(pageFile)) {
                    printer.accept(a, out);
                }
            }
        }
    }

    private String fileNameFor(final Note a) {
        // TODO
        Topic topic = new TopicDTO();
        topic.setId(Note.getId(a));
        return format.fileNameFor(topic);
    }

    public static boolean isMaterialized(final Note a) {
        return null != Note.getSource(a);
    }

    public static File chooseDirectoryForNote(final Note a, Map<String, File> dirs) {
        String source = Note.getSource(a);
        Preconditions.checkNotNull(source);
        File dir = dirs.get(source);
        Preconditions.checkNotNull(dir);
        return dir;
    }

    public static TreeNode<Link> noteToTreeMinimal(final Note note) {
        TreeNode<Link> tree = TreeNodeDTO.createEmptyNode();
        TreeViews.setId(tree, Note.getId(note));
        return tree;
    }

    public static TreeNode<Link> noteToTree(final Note note) {
        TreeNode<Link> tree = TreeNodeDTO.createEmptyNode();

        TreeViews.setId(tree, Note.getId(note));
        TreeViews.setAlias(tree, Note.getAlias(note));
        TreeViews.setCreated(tree, Note.getCreated(note));
        TreeViews.setPriority(tree, Note.getPriority(note));
        TreeViews.setShortcut(tree, Note.getShortcut(note));
        TreeViews.setSource(tree, Note.getSource(note));
        TreeViews.setText(tree, Note.getText(note));
        TreeViews.setTitle(tree, Note.getTitle(note));
        TreeViews.setWeight(tree, Note.getWeight(note));

        if (null != note.getChildren()) {
            List<Note> kids = ListNode.toJavaList(note.getChildren());
            TreeNode<Link>[] children = new TreeNode[kids.size()];
            int i = 0;
            for (Note kid : kids) {
                children[i++] = noteToTreeMinimal(kid);
            }
            tree.setChildren(ListNodeDTO.fromArray(children));
        }

        return tree;
    }

    public static Page noteToPage(final Note note) {
        TreeNode<Link> tree = new TreeNodeDTO<>();
        Topic topic = new TopicDTO();
        topic.setId(Note.getId(note));
        Link link = new LinkDTO();
        link.setTarget(topic);
        link.setLabel(Note.getTitle(note));
        tree.setValue(link);

        ListNode<Note> cur = note.getChildren();
        while (null != cur) {
            tree.addChild(noteToTreeMinimal(cur.getFirst()));
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

        return page;
    }
}
