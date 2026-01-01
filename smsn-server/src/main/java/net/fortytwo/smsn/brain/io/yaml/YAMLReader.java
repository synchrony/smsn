package net.fortytwo.smsn.brain.io.yaml;

import net.fortytwo.smsn.brain.Atom;
import net.fortytwo.smsn.brain.io.Format;
import net.fortytwo.smsn.brain.io.NoteReader;
import net.fortytwo.smsn.brain.io.vcs.FilePerNoteFormat;
import net.fortytwo.smsn.brain.repository.AtomRepositoryInterface;

import java.io.File;
import java.io.IOException;
import java.util.LinkedList;
import java.util.List;
import java.util.Map;

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
        AtomRepositoryInterface repository = context.getAtomRepository();

        YAMLGraph data = new YAMLGraph();
        for (Atom atom : data.readFrom(dirs)) {
            repository.save(atom);
        }
    }
}
