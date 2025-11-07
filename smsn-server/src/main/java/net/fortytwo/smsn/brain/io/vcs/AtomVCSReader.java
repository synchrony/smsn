package net.fortytwo.smsn.brain.io.vcs;

import net.fortytwo.smsn.SemanticSynchrony;
import net.fortytwo.smsn.brain.Atom;
import net.fortytwo.smsn.brain.AtomId;
import net.fortytwo.smsn.brain.io.NoteReader;
import net.fortytwo.smsn.brain.io.wiki.AtomWikiParser;
import net.fortytwo.smsn.brain.repository.AtomRepository;
import net.fortytwo.smsn.config.DataSource;

import java.io.File;
import java.io.FileInputStream;
import java.io.IOException;
import java.io.InputStream;
import java.util.List;

/**
 * VCS reader using new Atom format.
 * Reads .smsn files directly into Atom objects and saves to the repository.
 */
public class AtomVCSReader extends NoteReader {
    private static final FilePerNoteFormat FORMAT = new FilePerNoteFormat("VCS-Atom", "smsn");

    private final AtomRepository repository;
    private final AtomWikiParser parser;

    public AtomVCSReader(AtomRepository repository) {
        this.repository = repository;
        this.parser = new AtomWikiParser();
    }

    @Override
    protected void importInternal(Context context) throws IOException {
        for (DataSource source : SemanticSynchrony.getConfiguration().getSources()) {
            readDataSource(source);
        }
    }

    private void readDataSource(DataSource source) throws IOException {
        String location = source.getLocation();
        File dir = new File(location);

        if (!dir.exists()) {
            throw new IOException("Directory does not exist: " + location);
        }

        File[] files = dir.listFiles();
        if (files != null) {
            for (File file : files) {
                if (FORMAT.isMatchingFile(file)) {
                    try {
                        readAtomFile(file, source);
                    } catch (IOException e) {
                        throw new IOException("Failed to read file " + file.getAbsolutePath(), e);
                    }
                }
            }
        }
    }

    private void readAtomFile(File file, DataSource source) throws IOException {
        AtomId expectedId = idFromFileName(file);

        try (InputStream in = new FileInputStream(file)) {
            Atom atom = parser.parse(in);

            // Verify the ID matches the filename
            if (!atom.id.equals(expectedId)) {
                System.err.println("Warning: ID mismatch in file " + file.getName() +
                        " - expected " + expectedId.value + " but found " + atom.id.value);
            }

            // Override source from the datasource location
            atom = atom.withSource(new net.fortytwo.smsn.brain.SourceName(source.getName()));

            // Save to repository
            repository.save(atom);
        }
    }

    private AtomId idFromFileName(File file) {
        String fileName = file.getName();
        int dotIndex = fileName.lastIndexOf('.');
        String idPart = dotIndex > 0 ? fileName.substring(0, dotIndex) : fileName;
        return new AtomId(idPart);
    }

    @Override
    public List<net.fortytwo.smsn.brain.io.Format> getFormats() {
        return List.of(FORMAT);
    }
}
