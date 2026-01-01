package net.fortytwo.smsn.brain.repository;

import hydra.util.Opt;
import net.fortytwo.smsn.SemanticSynchrony;
import net.fortytwo.smsn.brain.Atom;
import net.fortytwo.smsn.brain.AtomId;
import net.fortytwo.smsn.brain.Normed;
import net.fortytwo.smsn.brain.SourceName;
import net.fortytwo.smsn.brain.Timestamp;
import net.fortytwo.smsn.brain.model.Filter;
import net.fortytwo.smsn.brain.model.pg.Sortable;

import java.io.File;
import java.io.IOException;
import java.sql.SQLException;
import java.util.ArrayList;
import java.util.Collections;
import java.util.Iterator;
import java.util.List;
import java.util.Optional;
import java.util.logging.Logger;
import java.util.stream.Collectors;

/**
 * File-based implementation of the atom repository.
 * Uses AtomFileStore for persistence and IndexManager for search/lookups.
 *
 * This provides the same functionality as AtomRepository but without
 * TinkerPop/Neo4j dependencies.
 */
public class FileBasedAtomRepository implements AtomRepositoryInterface, AutoCloseable {
    private static final Logger logger = SemanticSynchrony.getLogger();
    private static final int DEFAULT_MAX_SEARCH_RESULTS = 100;

    private final AtomFileStore fileStore;
    private final IndexManager indexManager;
    private long lastUpdate = System.currentTimeMillis();
    private boolean initialized = false;

    /**
     * Create a repository using the index directory from smsn.yaml configuration.
     */
    public FileBasedAtomRepository() throws IOException, SQLException {
        this(new File(SemanticSynchrony.getConfiguration().getIndexDirectory()));
    }

    /**
     * Create a repository with a specific index directory.
     */
    public FileBasedAtomRepository(File indexDirectory) throws IOException, SQLException {
        this.fileStore = new AtomFileStore();
        this.indexManager = new IndexManager(indexDirectory);
    }

    /**
     * Initialize the repository by loading all atoms and building the index.
     * Should be called once on startup.
     */
    public void initialize() throws IOException, SQLException {
        if (initialized) {
            return;
        }

        logger.info("Initializing file-based repository...");

        // Check if index is empty
        List<AtomId> existingIds = indexManager.getAllAtomIds();
        if (existingIds.isEmpty()) {
            // Load all atoms from files and index them using bulk operations
            logger.info("Building index from atom files...");
            long loadStart = System.currentTimeMillis();
            List<Atom> allAtoms = fileStore.loadAll();
            long loadElapsed = System.currentTimeMillis() - loadStart;
            logger.info("Loaded " + allAtoms.size() + " atoms from files in " + loadElapsed + "ms");

            // Use bulk indexing for much better performance
            indexManager.indexAtomsBulk(allAtoms);
            indexManager.commit();
            logger.info("Index built successfully");
        } else {
            logger.info("Using existing index with " + existingIds.size() + " atoms");
        }

        initialized = true;
    }

    // ========== CRUD Operations ==========

    /**
     * Load an atom by ID.
     */
    public Atom load(AtomId id) {
        Optional<Atom> atom = findById(id);
        if (atom.isEmpty()) {
            throw new IllegalArgumentException("No atom found with id: " + id.value);
        }
        return atom.get();
    }

    /**
     * Load an atom by ID, returning Optional.empty() if not found.
     */
    public Optional<Atom> findById(AtomId id) {
        Optional<Atom> atomOpt = fileStore.load(id);
        if (atomOpt.isPresent()) {
            // Load children from index for consistency
            try {
                List<AtomId> children = indexManager.getChildrenIds(id);
                Atom atom = atomOpt.get();
                return Optional.of(new Atom(
                    atom.id, atom.created, atom.weight, atom.priority,
                    atom.source, atom.title, atom.alias, atom.text, atom.shortcut,
                    children
                ));
            } catch (SQLException e) {
                logger.warning("Failed to load children for atom " + id.value + ": " + e.getMessage());
                return atomOpt;
            }
        }
        return atomOpt;
    }

    /**
     * Save an atom.
     */
    public void save(Atom atom) {
        try {
            fileStore.save(atom);
            indexManager.indexAtom(atom);
            notifyOfUpdate();
        } catch (SQLException | IOException e) {
            throw new RuntimeException("Failed to save atom: " + atom.id.value, e);
        }
    }

    /**
     * Delete an atom by ID.
     */
    public void delete(AtomId id) {
        try {
            fileStore.delete(id);
            indexManager.removeAtom(id);
            notifyOfUpdate();
        } catch (SQLException | IOException e) {
            throw new RuntimeException("Failed to delete atom: " + id.value, e);
        }
    }

    // ========== Graph Navigation ==========

    /**
     * Get the IDs of all children of an atom.
     */
    public List<AtomId> getChildrenIds(AtomId parentId) {
        try {
            return indexManager.getChildrenIds(parentId);
        } catch (SQLException e) {
            logger.warning("Failed to get children for " + parentId.value + ": " + e.getMessage());
            return Collections.emptyList();
        }
    }

    /**
     * Get all children atoms (fully loaded).
     */
    public List<Atom> getChildren(AtomId parentId) {
        return getChildrenIds(parentId).stream()
                .map(this::load)
                .collect(Collectors.toList());
    }

    /**
     * Set the children of an atom.
     */
    public void setChildren(AtomId parentId, List<AtomId> childIds) {
        try {
            indexManager.updateChildren(parentId, childIds);

            // Update the atom file to reflect children
            Optional<Atom> atomOpt = fileStore.load(parentId);
            if (atomOpt.isPresent()) {
                Atom atom = atomOpt.get();
                Atom updated = new Atom(
                    atom.id, atom.created, atom.weight, atom.priority,
                    atom.source, atom.title, atom.alias, atom.text, atom.shortcut,
                    childIds
                );
                fileStore.save(updated);
            }

            notifyOfUpdate();
        } catch (SQLException e) {
            throw new RuntimeException("Failed to set children for " + parentId.value, e);
        }
    }

    /**
     * Add a child at a specific position.
     */
    public void addChildAt(AtomId parentId, AtomId childId, int position) {
        try {
            List<AtomId> children = new ArrayList<>(indexManager.getChildrenIds(parentId));
            if (position < 0 || position > children.size()) {
                throw new IllegalArgumentException("Invalid position: " + position);
            }
            children.add(position, childId);
            setChildren(parentId, children);
        } catch (SQLException e) {
            throw new RuntimeException("Failed to add child to " + parentId.value, e);
        }
    }

    /**
     * Delete a child at a specific position.
     */
    public void deleteChildAt(AtomId parentId, int position) {
        try {
            List<AtomId> children = new ArrayList<>(indexManager.getChildrenIds(parentId));
            if (position < 0 || position >= children.size()) {
                throw new IllegalArgumentException("Invalid position: " + position);
            }
            children.remove(position);
            setChildren(parentId, children);
        } catch (SQLException e) {
            throw new RuntimeException("Failed to delete child from " + parentId.value, e);
        }
    }

    /**
     * Count the number of parents.
     */
    public int countParents(AtomId atomId) {
        try {
            return indexManager.countParents(atomId);
        } catch (SQLException e) {
            logger.warning("Failed to count parents for " + atomId.value + ": " + e.getMessage());
            return 0;
        }
    }

    /**
     * Get the IDs of all parent atoms.
     */
    public List<AtomId> getParentIds(AtomId atomId) {
        try {
            return indexManager.getParentIds(atomId);
        } catch (SQLException e) {
            logger.warning("Failed to get parents for " + atomId.value + ": " + e.getMessage());
            return Collections.emptyList();
        }
    }

    /**
     * Get all parent atoms (fully loaded).
     */
    public List<Atom> getParents(AtomId atomId) {
        return getParentIds(atomId).stream()
                .map(this::load)
                .collect(Collectors.toList());
    }

    // ========== Update Operations ==========

    /**
     * Update a single property of an atom.
     */
    public void updateProperty(AtomId atomId, String propertyKey, Object value) {
        Atom atom = load(atomId);
        Atom updated;

        switch (propertyKey) {
            case SemanticSynchrony.PropertyKeys.TITLE:
                updated = atom.withTitle((String) value);
                break;
            case SemanticSynchrony.PropertyKeys.TEXT:
                updated = atom.withText(value == null ? Opt.empty() : Opt.of((String) value));
                break;
            case SemanticSynchrony.PropertyKeys.WEIGHT:
                updated = atom.withWeight(new Normed(toFloat(value)));
                break;
            case SemanticSynchrony.PropertyKeys.PRIORITY:
                updated = atom.withPriority(value == null ? Opt.empty() : Opt.of(new Normed(toFloat(value))));
                break;
            case SemanticSynchrony.PropertyKeys.SOURCE:
                updated = atom.withSource(new SourceName((String) value));
                break;
            case SemanticSynchrony.PropertyKeys.SHORTCUT:
                updated = atom.withShortcut(value == null ? Opt.empty() : Opt.of((String) value));
                break;
            case SemanticSynchrony.PropertyKeys.ALIAS:
                updated = atom.withAlias(value == null ? Opt.empty() : Opt.of((String) value));
                break;
            default:
                throw new IllegalArgumentException("Unknown property: " + propertyKey);
        }

        save(updated);
    }

    private float toFloat(Object value) {
        if (value instanceof Float) {
            return (Float) value;
        } else if (value instanceof Double) {
            return ((Double) value).floatValue();
        } else if (value instanceof Number) {
            return ((Number) value).floatValue();
        } else if (value instanceof String) {
            return Float.parseFloat((String) value);
        } else {
            throw new IllegalArgumentException("Cannot convert " + value.getClass() + " to float");
        }
    }

    /**
     * Create a new atom with default values.
     */
    public Atom createAtom(AtomId id, SourceName source, String title) {
        Atom atom = new Atom(
                id,
                new Timestamp(System.currentTimeMillis()),
                new Normed(0.5f),
                Opt.empty(),
                source,
                title,
                Opt.empty(),
                Opt.empty(),
                Opt.empty(),
                new ArrayList<>()
        );
        save(atom);
        return atom;
    }

    /**
     * Create a new atom based on filter defaults.
     */
    public Atom createAtom(Filter filter) {
        AtomId newId = SemanticSynchrony.createRandomId();
        String defaultSource = filter != null ? filter.getDefaultSource() : null;
        SourceName source = defaultSource != null ? new SourceName(defaultSource) : new SourceName("default");
        float defaultWeight = filter != null ? filter.getDefaultWeight() : 0.5f;

        Atom atom = new Atom(
                newId,
                new Timestamp(System.currentTimeMillis()),
                new Normed(defaultWeight),
                Opt.empty(),
                source,
                "",
                Opt.empty(),
                Opt.empty(),
                Opt.empty(),
                new ArrayList<>()
        );
        save(atom);
        return atom;
    }

    // ========== Bulk Operations ==========

    /**
     * Get all atom IDs.
     */
    public List<AtomId> getAllAtomIds() {
        try {
            return indexManager.getAllAtomIds();
        } catch (SQLException e) {
            logger.warning("Failed to get all atom IDs: " + e.getMessage());
            return Collections.emptyList();
        }
    }

    /**
     * Get all atoms from a specific source.
     */
    public List<Atom> getAtomsBySource(String sourceName) {
        return fileStore.getAtomsBySource(sourceName);
    }

    // ========== Search Operations ==========

    /**
     * Search for atoms by full-text query on title.
     */
    public List<Atom> search(String query, Filter filter) {
        try {
            Iterator<Sortable<AtomId, Float>> results = indexManager.searchByTitle(query);
            return filterAndSort(results, filter, query);
        } catch (IOException e) {
            logger.warning("Search failed: " + e.getMessage());
            return Collections.emptyList();
        }
    }

    /**
     * Find atoms by shortcut.
     */
    public List<Atom> findByShortcut(String shortcut, Filter filter) {
        try {
            Optional<AtomId> id = indexManager.findByShortcut(shortcut);
            if (id.isPresent()) {
                Optional<Atom> atom = findById(id.get());
                if (atom.isPresent() && testFilter(atom.get(), filter)) {
                    return List.of(atom.get());
                }
            }
            return Collections.emptyList();
        } catch (SQLException e) {
            logger.warning("Shortcut search failed: " + e.getMessage());
            return Collections.emptyList();
        }
    }

    /**
     * Find atoms by acronym.
     */
    public List<Atom> findByAcronym(String acronym, Filter filter) {
        try {
            Iterator<Sortable<AtomId, Float>> results = indexManager.searchByAcronym(acronym);
            return filterAndSort(results, filter, acronym);
        } catch (IOException e) {
            logger.warning("Acronym search failed: " + e.getMessage());
            return Collections.emptyList();
        }
    }

    private List<Atom> filterAndSort(Iterator<Sortable<AtomId, Float>> results, Filter filter, String query) {
        List<Sortable<Atom, Float>> ranked = new ArrayList<>();

        while (results.hasNext()) {
            Sortable<AtomId, Float> result = results.next();
            Optional<Atom> atomOpt = findById(result.getEntity());
            if (atomOpt.isEmpty()) {
                continue;
            }

            Atom atom = atomOpt.get();
            if (!testFilter(atom, filter)) {
                continue;
            }

            float score = calculateScore(result.getScore(), atom, query);
            ranked.add(new Sortable<>(atom, score));
        }

        Collections.sort(ranked);

        return ranked.stream()
                .map(Sortable::getEntity)
                .limit(DEFAULT_MAX_SEARCH_RESULTS)
                .collect(Collectors.toList());
    }

    /**
     * Test if an atom passes the filter criteria.
     */
    public boolean testFilter(Atom atom, Filter filter) {
        if (filter == null || filter.isTrivial()) {
            return true;
        }

        if (atom.weight.value < filter.getMinWeight()) {
            return false;
        }

        java.util.Set<String> includedSources = filter.getIncludedSources();
        if (includedSources != null && !includedSources.isEmpty()) {
            if (!includedSources.contains(atom.source.value)) {
                return false;
            }
        }

        return true;
    }

    private float calculateScore(float nativeScore, Atom atom, String query) {
        float weight = atom.weight.value;
        float lengthPenalty = Math.min(1.0f, 1.0f * query.length() / Math.max(1, atom.title.length()));
        float priorityBonus = atom.priority.isPresent() ? 1f + atom.priority.get().value : 1f;
        return nativeScore * weight * lengthPenalty * priorityBonus;
    }

    // ========== Isolated Atoms ==========

    /**
     * Remove all isolated atoms (no parents and no children).
     */
    public void removeIsolatedAtoms(Filter filter) {
        List<AtomId> toRemove = new ArrayList<>();

        for (AtomId atomId : getAllAtomIds()) {
            Optional<Atom> atomOpt = findById(atomId);
            if (atomOpt.isEmpty()) {
                continue;
            }

            Atom atom = atomOpt.get();
            if (atom.children.isEmpty() && countParents(atomId) == 0) {
                if (filter == null || testFilter(atom, filter)) {
                    toRemove.add(atomId);
                }
            }
        }

        for (AtomId atomId : toRemove) {
            delete(atomId);
        }
    }

    // ========== Transaction Support ==========

    /**
     * Commit all pending changes.
     */
    public void commit() {
        try {
            fileStore.commit();
            indexManager.commit();
        } catch (IOException | SQLException e) {
            throw new RuntimeException("Failed to commit changes", e);
        }
    }

    /**
     * Rollback all pending changes.
     */
    public void rollback() {
        fileStore.rollback();
    }

    /**
     * Begin a transaction (for API compatibility).
     */
    public void begin() {
        // Currently a no-op; transactions are handled per-operation
    }

    // ========== Update Tracking ==========

    public void notifyOfUpdate() {
        lastUpdate = System.currentTimeMillis();
    }

    public long getLastUpdate() {
        return lastUpdate;
    }

    @Override
    public void close() throws Exception {
        commit();
        indexManager.close();
    }
}
