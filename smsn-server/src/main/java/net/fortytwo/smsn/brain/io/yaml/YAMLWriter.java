package net.fortytwo.smsn.brain.io.yaml;

import net.fortytwo.smsn.brain.Atom;
import net.fortytwo.smsn.brain.AtomId;
import net.fortytwo.smsn.brain.io.Format;
import net.fortytwo.smsn.brain.io.NoteWriter;
import net.fortytwo.smsn.brain.io.vcs.FilePerNoteFormat;
import net.fortytwo.smsn.brain.repository.AtomRepository;

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
        Map<String, File> dirs = FilePerNoteFormat.directoriesBySource();

        timeAction("exported atoms as individual files", () -> doExport(context.getAtomRepository(), dirs));
    }

    private void doExport(final AtomRepository repository, final Map<String, File> dirs) throws IOException {
        YAMLGraph data = new YAMLGraph();

        for (AtomId atomId : repository.getAllAtomIds()) {
            Atom atom = repository.load(atomId);
            data.addAtom(atom);
        }

        data.writeTo(dirs);
    }
}
