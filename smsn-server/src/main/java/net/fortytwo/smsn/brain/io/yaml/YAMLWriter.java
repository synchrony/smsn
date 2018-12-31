package net.fortytwo.smsn.brain.io.yaml;

import net.fortytwo.smsn.brain.io.Format;
import net.fortytwo.smsn.brain.io.NoteWriter;
import net.fortytwo.smsn.brain.io.vcs.VCSFormat;
import net.fortytwo.smsn.brain.model.TopicGraph;
import net.fortytwo.smsn.brain.model.entities.Note;

import java.io.File;
import java.io.IOException;
import java.util.LinkedList;
import java.util.List;
import java.util.Map;

public class YAMLWriter extends NoteWriter {
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
    public void doWrite(final Context context) throws IOException {
        Map<String, File> dirs = VCSFormat.getDirsBySource();

        timeAction("exported notes as individual files", () -> doExport(context.getTopicGraph(), dirs));
    }

    private void doExport(final TopicGraph graph, final Map<String, File> dirs) throws IOException {
        YAMLGraph data = new YAMLGraph();

        for (Note a : graph.getAllNotes()) {
            data.addAtom(a);
        }

        data.writeTo(dirs);
    }
}
