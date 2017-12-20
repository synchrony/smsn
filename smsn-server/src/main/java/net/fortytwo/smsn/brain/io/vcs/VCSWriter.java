package net.fortytwo.smsn.brain.io.vcs;

import com.google.common.base.Preconditions;
import net.fortytwo.smsn.brain.io.Format;
import net.fortytwo.smsn.brain.io.NoteWriter;
import net.fortytwo.smsn.brain.io.wiki.WikiPrinter;
import net.fortytwo.smsn.brain.model.Property;
import net.fortytwo.smsn.brain.model.TopicGraph;
import net.fortytwo.smsn.brain.model.dto.NoteDTO;
import net.fortytwo.smsn.brain.model.dto.TopicDTO;
import net.fortytwo.smsn.brain.model.entities.ListNode;
import net.fortytwo.smsn.brain.model.entities.Note;
import net.fortytwo.smsn.brain.model.entities.Topic;

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
            if (VCSFormat.isDataFile(file)) {
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
        return null != a.getSource();
    }

    private File chooseDirectoryForNote(final Note a, Map<String, File> dirs) {
        String source = a.getSource();
        Preconditions.checkNotNull(source);
        File dir = dirs.get(source);
        Preconditions.checkNotNull(dir);
        return dir;
    }

    private void writeNoteToStream(final Note note, final OutputStream out) {
        Note outNote = new NoteDTO();
        outNote.setTopic(note.getTopic());
        for (Property prop : Note.propertiesByKey.values()) {
            prop.getSetter().accept(outNote, prop.getGetter().apply(note));
        }

        if (null != note.getFirst()) {
            List<Note> children = new LinkedList<>();
            for (Note child : ListNode.toJavaList(note.getFirst())) {
                NoteDTO childCopy = new NoteDTO();
                childCopy.setTopic(child.getTopic());
                children.add(childCopy);
            }
            Note.setChildren(outNote, (Note[]) children.toArray());
        }

        new WikiPrinter(out).print(note);
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
