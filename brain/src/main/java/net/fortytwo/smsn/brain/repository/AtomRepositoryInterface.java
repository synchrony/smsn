package net.fortytwo.smsn.brain.repository;

import net.fortytwo.smsn.brain.Atom;
import net.fortytwo.smsn.brain.AtomId;
import net.fortytwo.smsn.brain.SourceName;
import net.fortytwo.smsn.brain.model.Filter;

import java.util.List;
import java.util.Optional;

/**
 * Interface for atom repository operations.
 * Provides abstraction over different storage backends (TinkerPop/Neo4j, file-based, etc.)
 */
public interface AtomRepositoryInterface {

    // ========== CRUD Operations ==========

    /**
     * Load an atom by ID.
     * @throws IllegalArgumentException if atom not found
     */
    Atom load(AtomId id);

    /**
     * Load an atom by ID, returning Optional.empty() if not found.
     */
    Optional<Atom> findById(AtomId id);

    /**
     * Save a new atom or update an existing one.
     */
    void save(Atom atom);

    /**
     * Delete an atom by ID.
     */
    void delete(AtomId id);

    // ========== Graph Navigation ==========

    /**
     * Get the IDs of all children of an atom.
     */
    List<AtomId> getChildrenIds(AtomId parentId);

    /**
     * Get all children atoms (fully loaded).
     */
    List<Atom> getChildren(AtomId parentId);

    /**
     * Set the children of an atom.
     */
    void setChildren(AtomId parentId, List<AtomId> childIds);

    /**
     * Add a child at a specific position.
     */
    void addChildAt(AtomId parentId, AtomId childId, int position);

    /**
     * Delete a child at a specific position.
     */
    void deleteChildAt(AtomId parentId, int position);

    /**
     * Count the number of parents.
     */
    int countParents(AtomId atomId);

    /**
     * Get the IDs of all parent atoms.
     */
    List<AtomId> getParentIds(AtomId atomId);

    /**
     * Get all parent atoms (fully loaded).
     */
    List<Atom> getParents(AtomId atomId);

    // ========== Update Operations ==========

    /**
     * Update a single property of an atom.
     */
    void updateProperty(AtomId atomId, String propertyKey, Object value);

    /**
     * Create a new atom with default values.
     */
    Atom createAtom(AtomId id, SourceName source, String title);

    /**
     * Create a new atom based on filter defaults.
     */
    Atom createAtom(Filter filter);

    // ========== Search Operations ==========

    /**
     * Search for atoms by full-text query on title.
     */
    List<Atom> search(String query, Filter filter);

    /**
     * Find atoms by shortcut.
     */
    List<Atom> findByShortcut(String shortcut, Filter filter);

    /**
     * Find atoms by acronym.
     */
    List<Atom> findByAcronym(String acronym, Filter filter);

    /**
     * Test if an atom passes the filter criteria.
     */
    boolean testFilter(Atom atom, Filter filter);

    // ========== Bulk Operations ==========

    /**
     * Get all atom IDs.
     */
    List<AtomId> getAllAtomIds();

    /**
     * Get all atoms from a specific source.
     */
    List<Atom> getAtomsBySource(String sourceName);

    /**
     * Remove all isolated atoms (no parents and no children).
     */
    void removeIsolatedAtoms(Filter filter);

    // ========== Transaction Support ==========

    /**
     * Commit all pending changes.
     */
    void commit();

    /**
     * Rollback all pending changes.
     */
    void rollback();

    /**
     * Begin a transaction.
     */
    void begin();

    // ========== Update Tracking ==========

    /**
     * Notify that an update has occurred.
     */
    void notifyOfUpdate();

    /**
     * Get the timestamp of the last update.
     */
    long getLastUpdate();
}
