package net.fortytwo.smsn.brain.io.vcs;

import net.fortytwo.smsn.SemanticSynchrony;
import net.fortytwo.smsn.brain.Atom;
import net.fortytwo.smsn.brain.AtomId;
import net.fortytwo.smsn.brain.io.NoteWriter;
import net.fortytwo.smsn.brain.io.wiki.AtomWikiPrinter;
import net.fortytwo.smsn.brain.repository.AtomRepositoryInterface;
import net.fortytwo.smsn.config.DataSource;

import java.io.File;
import java.io.FileOutputStream;
import java.io.IOException;
import java.io.OutputStream;
import java.util.HashMap;
import java.util.HashSet;
import java.util.List;
import java.util.Map;
import java.util.Set;

/**
 * VCS writer using new Atom format.
 * Writes .smsn files directly from Atom objects without the old Page/Note layer.
 */
public class AtomVCSWriter extends NoteWriter {
    public AtomVCSWriter() {
    }

    @Override
    public void doWrite(Context context) throws IOException {
        AtomRepositoryInterface repository = context.getAtomRepository();

        // Step 1: Scan all source directories and build index of existing atoms
        Map<String, Set<AtomId>> existingAtomsBySource = scanExistingAtoms();

        // Step 2: Write atoms, handling moves between sources
        for (DataSource source : SemanticSynchrony.getConfiguration().getSources()) {
            writeDataSource(source, repository, existingAtomsBySource);
        }

        // Step 3: Clean up orphaned atoms (atoms that exist on disk but not in graph)
        cleanupOrphanedAtoms(existingAtomsBySource);
    }

    /**
     * Scan all source directories and return a map of source name -> set of atom IDs.
     */
    private Map<String, Set<AtomId>> scanExistingAtoms() {
        Map<String, Set<AtomId>> result = new HashMap<>();

        for (DataSource source : SemanticSynchrony.getConfiguration().getSources()) {
            Set<AtomId> atomIds = new HashSet<>();
            File dir = new File(source.getLocation());

            if (dir.exists() && dir.isDirectory()) {
                File[] files = dir.listFiles();
                if (files != null) {
                    for (File file : files) {
                        if (VCSFormat.FORMAT.isMatchingFile(file)) {
                            String filename = file.getName();
                            int dotIndex = filename.lastIndexOf('.');
                            String idStr = dotIndex > 0 ? filename.substring(0, dotIndex) : filename;
                            atomIds.add(new AtomId(idStr));
                        }
                    }
                }
            }

            result.put(source.getName(), atomIds);
        }

        return result;
    }

    private void writeDataSource(DataSource source, AtomRepositoryInterface repository,
                                  Map<String, Set<AtomId>> existingAtomsBySource) throws IOException {
        String location = source.getLocation();
        File dir = new File(location);

        if (!dir.exists() && !dir.mkdirs()) {
            throw new IOException("Failed to create directory: " + location);
        }

        // Get all atoms from this source and write them
        List<Atom> atoms = repository.getAtomsBySource(source.getName());
        for (Atom atom : atoms) {
            writeAtom(atom, source.getName(), dir, existingAtomsBySource);
        }
    }

    private void writeAtom(Atom atom, String correctSource, File directory,
                          Map<String, Set<AtomId>> existingAtomsBySource) throws IOException {
        String filename = atom.id.value + ".smsn";

        // Delete from wrong sources if this atom has moved
        for (Map.Entry<String, Set<AtomId>> entry : existingAtomsBySource.entrySet()) {
            String sourceName = entry.getKey();
            Set<AtomId> atomIds = entry.getValue();

            if (!sourceName.equals(correctSource) && atomIds.contains(atom.id)) {
                // This atom exists in the wrong source directory - delete it
                File wrongDir = getDirectoryForSource(sourceName);
                File wrongFile = new File(wrongDir, filename);
                if (wrongFile.exists()) {
                    wrongFile.delete();
                }
                atomIds.remove(atom.id);
            }
        }

        // Write to correct location
        File file = new File(directory, filename);
        try (OutputStream out = new FileOutputStream(file)) {
            new AtomWikiPrinter(out).print(atom);
        }

        // Mark this atom as written (remove from cleanup set)
        Set<AtomId> correctSourceSet = existingAtomsBySource.get(correctSource);
        if (correctSourceSet != null) {
            correctSourceSet.remove(atom.id);
        }
    }

    /**
     * Delete any atoms that exist on disk but weren't written (orphaned atoms).
     */
    private void cleanupOrphanedAtoms(Map<String, Set<AtomId>> existingAtomsBySource) {
        for (Map.Entry<String, Set<AtomId>> entry : existingAtomsBySource.entrySet()) {
            String sourceName = entry.getKey();
            Set<AtomId> orphanedIds = entry.getValue();

            if (!orphanedIds.isEmpty()) {
                File dir = getDirectoryForSource(sourceName);
                for (AtomId atomId : orphanedIds) {
                    File orphanedFile = new File(dir, atomId.value + ".smsn");
                    if (orphanedFile.exists()) {
                        orphanedFile.delete();
                    }
                }
            }
        }
    }

    private File getDirectoryForSource(String sourceName) {
        for (DataSource source : SemanticSynchrony.getConfiguration().getSources()) {
            if (source.getName().equals(sourceName)) {
                return new File(source.getLocation());
            }
        }
        throw new IllegalArgumentException("Unknown source: " + sourceName);
    }

    @Override
    public List<net.fortytwo.smsn.brain.io.Format> getFormats() {
        return List.of(VCSFormat.FORMAT);
    }
}
