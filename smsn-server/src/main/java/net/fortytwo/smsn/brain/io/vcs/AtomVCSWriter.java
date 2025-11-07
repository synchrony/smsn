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
    public static final FilePerNoteFormat FORMAT = new FilePerNoteFormat("VCS-Atom", "smsn");

    public AtomVCSWriter() {
    }

    @Override
    public void doWrite(Context context) throws IOException {
        AtomRepository repository = context.getAtomRepository();
        for (DataSource source : SemanticSynchrony.getConfiguration().getSources()) {
            writeDataSource(source, repository);
        }
    }

    private void writeDataSource(DataSource source, AtomRepository repository) throws IOException {
        String location = source.getLocation();
        File dir = new File(location);

        if (!dir.exists() && !dir.mkdirs()) {
            throw new IOException("Failed to create directory: " + location);
        }

        // Get all atoms from this source and write them
        List<Atom> atoms = repository.getAtomsBySource(source.getName());
        for (Atom atom : atoms) {
            writeAtom(atom, dir);
        }
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
