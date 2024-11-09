package net.fortytwo.smsn.brain.io.yaml;

import net.fortytwo.smsn.brain.io.Format;
import net.fortytwo.smsn.brain.io.NoteReader;
import net.fortytwo.smsn.brain.io.vcs.FilePerNoteFormat;
import net.fortytwo.smsn.brain.model.Filter;
import net.fortytwo.smsn.brain.model.entities.Link;
import net.fortytwo.smsn.brain.model.entities.Note;
import net.fortytwo.smsn.brain.model.entities.TreeNode;
import net.fortytwo.smsn.brain.query.ViewStyle;

import java.io.File;
import java.io.IOException;
import java.util.LinkedList;
import java.util.List;
import java.util.Map;

import static net.fortytwo.smsn.brain.io.vcs.VCSWriter.noteToTree;

public class YAMLReader extends NoteReader {
    private static final List<Format> formats;

    static {
        formats = new LinkedList<>();
        formats.add(YAMLFormat.getInstance());
    }

    @Override
    public List<Format> getFormats() {
        return formats;
    }

    @Override
    protected void importInternal(final Context context) throws IOException {
        Map<String, File> dirs = FilePerNoteFormat.directoriesBySource();

        YAMLGraph data = new YAMLGraph();
        for (Note root : data.readFrom(dirs)) {
            TreeNode<Link> node = noteToTree(root);
            context.getQueries().update(node, 1, Filter.simpleFilter(), ViewStyle.Basic.Forward.getStyle());
            checkAndCommit(context.getTopicGraph());
        }
    }
}
