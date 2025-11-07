package net.fortytwo.smsn.brain.io.vcs;

import net.fortytwo.smsn.SemanticSynchrony;
import net.fortytwo.smsn.brain.Atom;
import net.fortytwo.smsn.brain.AtomId;
import net.fortytwo.smsn.brain.io.NoteWriter;
import net.fortytwo.smsn.brain.io.wiki.AtomWikiPrinter;
import net.fortytwo.smsn.brain.repository.AtomRepository;
import net.fortytwo.smsn.config.DataSource;

import java.io.File;
import java.io.FileOutputStream;
import java.io.IOException;
import java.io.OutputStream;
import java.util.List;

/**
 * VCS writer using new Atom format.
 * Writes .smsn files directly from Atom objects without the old Page/Note layer.
 */
public class AtomVCSWriter extends NoteWriter {
    private static final FilePerNoteFormat FORMAT = new FilePerNoteFormat("VCS-Atom", "smsn");

    private final AtomRepository repository;

    public AtomVCSWriter(AtomRepository repository) {
        this.repository = repository;
    }

    @Override
    public void doWrite(Context context) throws IOException {
        for (DataSource source : SemanticSynchrony.getConfiguration().getSources()) {
            writeDataSource(source);
        }
    }

    private void writeDataSource(DataSource source) throws IOException {
        String location = source.getLocation();
        File dir = new File(location);

        if (!dir.exists() && !dir.mkdirs()) {
            throw new IOException("Failed to create directory: " + location);
        }

        // Get all atoms from this source and write them
        // Note: This requires a way to query atoms by source, which we'll need to add
        // For now, this is a placeholder showing the structure
    }

    public void writeAtom(Atom atom, File directory) throws IOException {
        String filename = atom.id.value + ".smsn";
        File file = new File(directory, filename);

        try (OutputStream out = new FileOutputStream(file)) {
            new AtomWikiPrinter(out).print(atom);
        }
    }

    @Override
    public List<net.fortytwo.smsn.brain.io.Format> getFormats() {
        return List.of(FORMAT);
    }
}
