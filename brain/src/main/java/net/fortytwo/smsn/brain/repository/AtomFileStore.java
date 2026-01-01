package net.fortytwo.smsn.brain.repository;

import net.fortytwo.smsn.SemanticSynchrony;
import net.fortytwo.smsn.brain.Atom;
import net.fortytwo.smsn.brain.AtomId;
import net.fortytwo.smsn.brain.SourceName;
import net.fortytwo.smsn.brain.io.wiki.AtomWikiParser;
import net.fortytwo.smsn.brain.io.wiki.AtomWikiPrinter;
import net.fortytwo.smsn.config.DataSource;

import java.io.File;
import java.io.FileInputStream;
import java.io.FileOutputStream;
import java.io.IOException;
import java.io.InputStream;
import java.io.OutputStream;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.Optional;
import java.util.concurrent.ConcurrentHashMap;
import java.util.logging.Logger;

/**
 * File-based storage for atoms using the .smsn wiki format.
 * Each atom is stored as a separate file: {source-location}/{atom-id}.smsn
 *
 * This class handles reading/writing individual atom files and maintains
 * an in-memory cache for performance.
 */
public class AtomFileStore {
    private static final Logger logger = SemanticSynchrony.getLogger();
    private static final String FILE_EXTENSION = ".smsn";

    private final AtomWikiParser parser;
    private final Map<String, File> sourceDirectories;

    // In-memory cache of loaded atoms
    private final Map<AtomId, Atom> cache;

    // Track which atoms have been modified
    private final Map<AtomId, Atom> pendingWrites;

    // Track atoms to be deleted
    private final Map<AtomId, String> pendingDeletes; // atomId -> source

    public AtomFileStore() {
        this.parser = new AtomWikiParser();
        this.sourceDirectories = new HashMap<>();
        this.cache = new ConcurrentHashMap<>();
        this.pendingWrites = new ConcurrentHashMap<>();
        this.pendingDeletes = new ConcurrentHashMap<>();

        initializeSourceDirectories();
    }

    private void initializeSourceDirectories() {
        for (DataSource source : SemanticSynchrony.getConfiguration().getSources()) {
            File dir = new File(source.getLocation());
            if (!dir.exists()) {
                if (!dir.mkdirs()) {
                    logger.warning("Failed to create source directory: " + source.getLocation());
                }
            }
            sourceDirectories.put(source.getName(), dir);
        }
    }

    /**
     * Load an atom from file.
     * Returns cached version if available.
     */
    public Optional<Atom> load(AtomId id) {
        // Check cache first
        Atom cached = cache.get(id);
        if (cached != null) {
            return Optional.of(cached);
        }

        // Check pending writes
        Atom pending = pendingWrites.get(id);
        if (pending != null) {
            return Optional.of(pending);
        }

        // Search in all source directories
        for (Map.Entry<String, File> entry : sourceDirectories.entrySet()) {
            String sourceName = entry.getKey();
            File dir = entry.getValue();
            File file = new File(dir, id.value + FILE_EXTENSION);

            if (file.exists()) {
                try {
                    Atom atom = loadFromFile(file, sourceName);
                    cache.put(id, atom);
                    return Optional.of(atom);
                } catch (IOException e) {
                    logger.warning("Failed to load atom from " + file + ": " + e.getMessage());
                }
            }
        }

        return Optional.empty();
    }

    /**
     * Save an atom. The atom is added to pending writes and will be
     * persisted to disk on commit().
     */
    public void save(Atom atom) {
        pendingWrites.put(atom.id, atom);
        cache.put(atom.id, atom);

        // Remove from pending deletes if it was there
        pendingDeletes.remove(atom.id);
    }

    /**
     * Mark an atom for deletion.
     */
    public void delete(AtomId id) {
        // Find the source for this atom
        Optional<Atom> existing = load(id);
        if (existing.isPresent()) {
            pendingDeletes.put(id, existing.get().source.value);
        }

        cache.remove(id);
        pendingWrites.remove(id);
    }

    /**
     * Commit all pending changes to disk.
     */
    public void commit() throws IOException {
        // Write all pending atoms
        for (Atom atom : pendingWrites.values()) {
            writeAtomToFile(atom);
        }
        pendingWrites.clear();

        // Delete marked atoms
        for (Map.Entry<AtomId, String> entry : pendingDeletes.entrySet()) {
            AtomId id = entry.getKey();
            String sourceName = entry.getValue();
            File dir = sourceDirectories.get(sourceName);
            if (dir != null) {
                File file = new File(dir, id.value + FILE_EXTENSION);
                if (file.exists()) {
                    if (!file.delete()) {
                        logger.warning("Failed to delete atom file: " + file);
                    }
                }
            }
        }
        pendingDeletes.clear();
    }

    /**
     * Discard all pending changes.
     */
    public void rollback() {
        // Reload any atoms that were modified from disk
        for (AtomId id : pendingWrites.keySet()) {
            cache.remove(id);
        }
        pendingWrites.clear();
        pendingDeletes.clear();
    }

    /**
     * Load all atoms from all source directories.
     * Used for initial indexing.
     */
    public List<Atom> loadAll() {
        List<Atom> atoms = new ArrayList<>();

        for (Map.Entry<String, File> entry : sourceDirectories.entrySet()) {
            String sourceName = entry.getKey();
            File dir = entry.getValue();

            if (!dir.exists() || !dir.isDirectory()) {
                continue;
            }

            File[] files = dir.listFiles();
            if (files == null) {
                continue;
            }

            for (File file : files) {
                if (file.getName().endsWith(FILE_EXTENSION)) {
                    try {
                        Atom atom = loadFromFile(file, sourceName);
                        atoms.add(atom);
                        cache.put(atom.id, atom);
                    } catch (IOException e) {
                        logger.warning("Failed to load " + file + ": " + e.getMessage());
                    }
                }
            }
        }

        return atoms;
    }

    /**
     * Get all atoms for a specific source.
     */
    public List<Atom> getAtomsBySource(String sourceName) {
        List<Atom> atoms = new ArrayList<>();
        File dir = sourceDirectories.get(sourceName);

        if (dir == null || !dir.exists() || !dir.isDirectory()) {
            return atoms;
        }

        File[] files = dir.listFiles();
        if (files == null) {
            return atoms;
        }

        for (File file : files) {
            if (file.getName().endsWith(FILE_EXTENSION)) {
                try {
                    Atom atom = loadFromFile(file, sourceName);
                    atoms.add(atom);
                } catch (IOException e) {
                    logger.warning("Failed to load " + file + ": " + e.getMessage());
                }
            }
        }

        return atoms;
    }

    private Atom loadFromFile(File file, String sourceName) throws IOException {
        try (InputStream in = new FileInputStream(file)) {
            Atom atom = parser.parse(in);
            // Override source from directory
            return atom.withSource(new SourceName(sourceName));
        }
    }

    private void writeAtomToFile(Atom atom) throws IOException {
        File dir = sourceDirectories.get(atom.source.value);
        if (dir == null) {
            throw new IOException("Unknown source: " + atom.source.value);
        }

        // Handle source change - delete from old location first
        for (Map.Entry<String, File> entry : sourceDirectories.entrySet()) {
            if (!entry.getKey().equals(atom.source.value)) {
                File oldFile = new File(entry.getValue(), atom.id.value + FILE_EXTENSION);
                if (oldFile.exists()) {
                    oldFile.delete();
                }
            }
        }

        File file = new File(dir, atom.id.value + FILE_EXTENSION);
        try (OutputStream out = new FileOutputStream(file)) {
            new AtomWikiPrinter(out).print(atom);
        }
    }

    /**
     * Check if an atom exists (in cache, pending, or on disk).
     */
    public boolean exists(AtomId id) {
        if (cache.containsKey(id) || pendingWrites.containsKey(id)) {
            return !pendingDeletes.containsKey(id);
        }
        return load(id).isPresent();
    }

    /**
     * Clear the in-memory cache.
     */
    public void clearCache() {
        cache.clear();
    }
}
