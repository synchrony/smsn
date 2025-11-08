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
import java.util.ArrayList;
import java.util.List;

/**
 * VCS reader using new Atom format.
 * Reads .smsn files directly into Atom objects and saves to the repository.
 */
public class AtomVCSReader extends NoteReader {
    private final AtomWikiParser parser;

    public AtomVCSReader() {
        this.parser = new AtomWikiParser();
    }

    @Override
    protected void importInternal(Context context) throws IOException {
        AtomRepository repository = context.getAtomRepository();

        // Two-pass import:
        // Pass 1: Read and save all atoms (properties only)
        List<Atom> allAtoms = new ArrayList<>();
        int totalFiles = 0;

        logger.info("Pass 1: Reading atom properties...");
        for (DataSource source : SemanticSynchrony.getConfiguration().getSources()) {
            int filesRead = readDataSource(source, repository, context, allAtoms);
            totalFiles += filesRead;
            logger.info("Read " + filesRead + " atoms from source: " + source.getName());
        }
        logger.info("Total atoms imported: " + totalFiles);

        // Commit all changes from pass 1 so vertices are visible in the index
        logger.info("Committing pass 1 changes...");
        context.getTopicGraph().commit();
        context.getTopicGraph().begin();

        // Verify atoms are findable after commit
        logger.info("Verifying atoms are indexed...");
        int notFound = 0;
        for (Atom atom : allAtoms) {
            if (repository.findById(atom.id).isEmpty()) {
                logger.warning("Atom not found after commit: " + atom.id.value);
                notFound++;
                if (notFound > 10) {
                    logger.warning("... and " + (allAtoms.size() - notFound) + " more");
                    break;
                }
            }
        }
        if (notFound == 0) {
            logger.info("All " + allAtoms.size() + " atoms verified in index");
        } else {
            logger.warning(notFound + " atoms not found in index after commit!");
        }

        // Pass 2: Set all children relationships
        logger.info("Pass 2: Creating child relationships...");
        int relationshipsCreated = 0;
        int errors = 0;
        for (Atom atom : allAtoms) {
            if (!atom.children.isEmpty()) {
                try {
                    repository.setChildren(atom.id, atom.children);
                    relationshipsCreated += atom.children.size();
                } catch (IllegalArgumentException e) {
                    // Log but continue - might be dangling references
                    logger.warning("Failed to set children for atom " + atom.id.value + ": " + e.getMessage());
                    errors++;
                }
            }
            checkAndCommit(context.getTopicGraph());
        }
        logger.info("Created " + relationshipsCreated + " child relationships" +
                (errors > 0 ? " (" + errors + " errors)" : ""));

        // Final commit for pass 2 changes
        logger.info("Committing pass 2 changes...");
        context.getTopicGraph().commit();
    }

    private int readDataSource(DataSource source, AtomRepository repository, Context context,
                               List<Atom> allAtoms) throws IOException {
        String location = source.getLocation();
        File dir = new File(location);

        if (!dir.exists()) {
            logger.warning("Directory does not exist: " + location);
            return 0;
        }

        if (!dir.isDirectory()) {
            logger.warning("Not a directory: " + location);
            return 0;
        }

        File[] files = dir.listFiles();
        if (files == null) {
            logger.warning("Cannot list files in directory: " + location);
            return 0;
        }

        int count = 0;
        for (File file : files) {
            if (VCSFormat.FORMAT.isMatchingFile(file)) {
                try {
                    Atom atom = readAtomFile(file, source, repository);
                    allAtoms.add(atom);
                    count++;

                    // Commit periodically to avoid large transactions
                    checkAndCommit(context.getTopicGraph());
                } catch (IOException e) {
                    throw new IOException("Failed to read file " + file.getAbsolutePath(), e);
                }
            }
        }
        return count;
    }

    private Atom readAtomFile(File file, DataSource source, AtomRepository repository) throws IOException {
        AtomId expectedId = idFromFileName(file);

        try (InputStream in = new FileInputStream(file)) {
            Atom atom = parser.parse(in);

            if (atom == null) {
                throw new IOException("Parser returned null for file: " + file.getAbsolutePath());
            }

            // Verify the ID matches the filename
            if (!atom.id.equals(expectedId)) {
                logger.warning("ID mismatch in file " + file.getName() +
                        " - expected " + expectedId.value + " but found " + atom.id.value);
            }

            // Override source from the datasource location
            atom = atom.withSource(new net.fortytwo.smsn.brain.SourceName(source.getName()));

            // Save to repository (properties only, children will be set in pass 2)
            repository.save(atom);

            return atom;
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
        return List.of(VCSFormat.FORMAT);
    }
}
