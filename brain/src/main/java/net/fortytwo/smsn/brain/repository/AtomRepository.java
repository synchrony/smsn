package net.fortytwo.smsn.brain.repository;

import hydra.util.Opt;
import net.fortytwo.smsn.SemanticSynchrony;
import net.fortytwo.smsn.brain.Atom;
import net.fortytwo.smsn.brain.AtomId;
import net.fortytwo.smsn.brain.Normed;
import net.fortytwo.smsn.brain.SourceName;
import net.fortytwo.smsn.brain.Timestamp;
import net.fortytwo.smsn.brain.model.Filter;
import net.fortytwo.smsn.brain.model.pg.GraphWrapper;
import org.apache.tinkerpop.gremlin.structure.Direction;
import org.apache.tinkerpop.gremlin.structure.Vertex;
import org.apache.tinkerpop.gremlin.structure.VertexProperty;

import java.util.ArrayList;
import java.util.Collections;
import java.util.Iterator;
import java.util.List;
import java.util.Optional;
import java.util.stream.Collectors;

/**
 * Repository for loading and saving Atom instances to/from the graph database.
 * Provides a clean abstraction over the TinkerPop/Neo4j graph implementation.
 */
public class AtomRepository {
    private final GraphWrapper wrapper;

    public AtomRepository(GraphWrapper wrapper) {
        this.wrapper = wrapper;
    }

    // ========== CRUD Operations ==========

    /**
     * Load an atom by ID from the graph.
     */
    public Atom load(AtomId id) {
        Vertex v = wrapper.getVertexById(id);
        if (v == null) {
            throw new IllegalArgumentException("No atom found with id: " + id.value);
        }
        return vertexToAtom(v);
    }

    /**
     * Load an atom by ID, returning Optional.empty() if not found.
     */
    public Optional<Atom> findById(AtomId id) {
        Vertex v = wrapper.getVertexById(id);
        return v == null ? Optional.empty() : Optional.of(vertexToAtom(v));
    }

    /**
     * Save a new atom to the graph or update an existing one.
     */
    public void save(Atom atom) {
        Vertex v = wrapper.getVertexById(atom.id);
        if (v == null) {
            v = wrapper.getGraph().addVertex();
        }
        atomToVertex(atom, v);
        updateAllIndices(v);
    }

    /**
     * Delete an atom by ID.
     */
    public void delete(AtomId id) {
        Vertex v = wrapper.getVertexById(id);
        if (v != null) {
            v.remove();
        }
    }

    // ========== Graph Navigation ==========

    /**
     * Get the IDs of all children of an atom.
     * Children are stored as a linked list structure in the graph.
     */
    public List<AtomId> getChildrenIds(AtomId parentId) {
        Vertex parent = wrapper.getVertexById(parentId);
        if (parent == null) {
            return Collections.emptyList();
        }
        return collectChildIds(parent);
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
        Vertex parent = wrapper.getVertexById(parentId);
        if (parent == null) {
            throw new IllegalArgumentException("Parent atom not found: " + parentId.value);
        }

        // Remove existing children list
        removeAllChildren(parent);

        // Create new children list
        if (!childIds.isEmpty()) {
            createChildrenList(parent, childIds);
        }
    }

    /**
     * Count the number of parents (atoms that have this atom as a child).
     */
    public int countParents(AtomId atomId) {
        Vertex atom = wrapper.getVertexById(atomId);
        if (atom == null) {
            return 0;
        }

        int count = 0;
        // Navigate backwards through FIRST edges
        var firstEdges = atom.edges(Direction.IN, SemanticSynchrony.EdgeLabels.FIRST);
        while (firstEdges.hasNext()) {
            firstEdges.next();
            count++;
        }
        return count;
    }

    // ========== Update Operations ==========

    /**
     * Update a single property of an atom.
     * Handles type conversions automatically (e.g., Double to Float from JSON).
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

    /**
     * Convert various numeric types to float.
     * Handles JSON deserialization which produces Doubles.
     */
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
                new Timestamp((int) (System.currentTimeMillis() / 1000)),
                new Normed(0.5f),  // default weight
                Opt.empty(),       // no priority
                source,
                title,
                Opt.empty(),       // no alias
                Opt.empty(),       // no text
                Opt.empty(),       // no shortcut
                new ArrayList<>()  // no children
        );
        save(atom);
        return atom;
    }

    /**
     * Load an atom with all its children recursively.
     * Returns the atom with children list populated.
     */
    public Atom loadWithChildren(AtomId id) {
        return load(id);
        // Note: children IDs are already included in the Atom
        // If we want to load the full child Atoms, we'd need to:
        // 1. Get the atom
        // 2. Load each child
        // This is intentionally left simple - caller can load children if needed
    }

    // ========== Bulk Operations ==========

    /**
     * Get all atom IDs in the graph.
     */
    public List<AtomId> getAllAtomIds() {
        List<AtomId> ids = new ArrayList<>();
        wrapper.getGraph().vertices().forEachRemaining(v -> {
            String idValue = getOptionalProperty(v, SemanticSynchrony.PropertyKeys.ID, null);
            if (idValue != null) {
                ids.add(new AtomId(idValue));
            }
        });
        return ids;
    }

    /**
     * Get all atoms from a specific source.
     */
    public List<Atom> getAtomsBySource(String sourceName) {
        List<Atom> atoms = new ArrayList<>();
        wrapper.getGraph().vertices().forEachRemaining(v -> {
            String source = getOptionalProperty(v, SemanticSynchrony.PropertyKeys.SOURCE, null);
            if (sourceName.equals(source)) {
                atoms.add(vertexToAtom(v));
            }
        });
        return atoms;
    }

    // ========== Search Operations ==========

    /**
     * Search for atoms by full-text query on title.
     */
    public List<Atom> search(String query, Filter filter) {
        return filterAndSort(wrapper.getVerticesByTitle(query), filter, query);
    }

    /**
     * Find atoms by shortcut.
     */
    public List<Atom> findByShortcut(String shortcut, Filter filter) {
        return filterAndSort(wrapper.getVerticesByShortcut(shortcut), filter, shortcut);
    }

    /**
     * Find atoms by acronym.
     */
    public List<Atom> findByAcronym(String acronym, Filter filter) {
        return filterAndSort(wrapper.getVerticesByAcronym(acronym.toLowerCase()), filter, acronym);
    }

    /**
     * Filter and sort search results by relevance score.
     * Combines native search score with weight, priority, and title length.
     */
    private List<Atom> filterAndSort(
            Iterator<net.fortytwo.smsn.brain.model.pg.Sortable<Vertex, Float>> unranked,
            Filter filter,
            String query) {

        List<net.fortytwo.smsn.brain.model.pg.Sortable<Atom, Float>> ranked = new ArrayList<>();

        while (unranked.hasNext()) {
            net.fortytwo.smsn.brain.model.pg.Sortable<Vertex, Float> in = unranked.next();
            Atom atom = vertexToAtom(in.getEntity());

            // Apply filter
            if (!testFilter(atom, filter)) {
                continue;
            }

            // Calculate relevance score
            float score = calculateScore(in, atom, query);
            ranked.add(new net.fortytwo.smsn.brain.model.pg.Sortable<>(atom, score));
        }

        // Sort by score (descending)
        Collections.sort(ranked);

        // Extract atoms from sorted list
        return ranked.stream()
                .map(net.fortytwo.smsn.brain.model.pg.Sortable::getEntity)
                .collect(Collectors.toList());
    }

    /**
     * Test if an atom passes the filter criteria.
     * Checks source and weight against filter settings.
     */
    private boolean testFilter(Atom atom, Filter filter) {
        if (filter == null || filter.isTrivial()) {
            return true;
        }

        // Check weight
        if (atom.weight.value < filter.getMinWeight()) {
            return false;
        }

        // Check source
        String minSource = filter.getMinSource();
        if (minSource != null) {
            // Source comparison based on configuration order
            Integer atomSourceIndex = getSourceIndex(atom.source.value);
            Integer minSourceIndex = getSourceIndex(minSource);
            if (atomSourceIndex == null || minSourceIndex == null || atomSourceIndex < minSourceIndex) {
                return false;
            }
        }

        return true;
    }

    /**
     * Get the index of a source from configuration.
     */
    private Integer getSourceIndex(String sourceName) {
        List<net.fortytwo.smsn.config.DataSource> sources = SemanticSynchrony.getConfiguration().getSources();
        for (int i = 0; i < sources.size(); i++) {
            if (sources.get(i).getName().equals(sourceName)) {
                return i;
            }
        }
        return null;
    }

    /**
     * Calculate relevance score for search results.
     * Combines native search score with weight, priority, and title length penalty.
     */
    private float calculateScore(
            net.fortytwo.smsn.brain.model.pg.Sortable<Vertex, Float> in,
            Atom atom,
            String query) {

        float nativeScore = in.getScore();
        float weight = atom.weight.value;

        // Penalize longer titles (prefer shorter, more concise matches)
        float lengthPenalty = Math.min(1.0f, 1.0f * query.length() / Math.max(1, atom.title.length()));

        // Boost by priority if present
        float priorityBonus = atom.priority.isPresent()
                ? 1f + atom.priority.get().value
                : 1f;

        return nativeScore * weight * lengthPenalty * priorityBonus;
    }

    // ========== Conversion Methods ==========

    /**
     * Convert a graph Vertex to an Atom.
     */
    private Atom vertexToAtom(Vertex v) {
        // Handle CREATED property - stored as Long for compatibility, but Timestamp expects int
        Object createdValue = getRequiredProperty(v, SemanticSynchrony.PropertyKeys.CREATED);
        int created = createdValue instanceof Long
                ? ((Long) createdValue).intValue()
                : (Integer) createdValue;

        return new Atom(
                new AtomId(getRequiredProperty(v, SemanticSynchrony.PropertyKeys.ID)),
                new Timestamp(created),
                new Normed(getOptionalProperty(v, SemanticSynchrony.PropertyKeys.WEIGHT, 0.5f)),
                optionalNormed(getOptionalProperty(v, SemanticSynchrony.PropertyKeys.PRIORITY, null)),
                new SourceName(getRequiredProperty(v, SemanticSynchrony.PropertyKeys.SOURCE)),
                getOptionalProperty(v, SemanticSynchrony.PropertyKeys.TITLE, ""),
                optional(getOptionalProperty(v, SemanticSynchrony.PropertyKeys.ALIAS, null)),
                optional(getOptionalProperty(v, SemanticSynchrony.PropertyKeys.TEXT, null)),
                optional(getOptionalProperty(v, SemanticSynchrony.PropertyKeys.SHORTCUT, null)),
                collectChildIds(v)
        );
    }

    /**
     * Write an Atom's properties to a Vertex.
     */
    private void atomToVertex(Atom atom, Vertex v) {
        v.property(SemanticSynchrony.PropertyKeys.ID, atom.id.value);
        // Store CREATED as Long for compatibility with old Note interface
        v.property(SemanticSynchrony.PropertyKeys.CREATED, (long) atom.created.value);
        v.property(SemanticSynchrony.PropertyKeys.WEIGHT, atom.weight.value);

        setOptionalProperty(v, SemanticSynchrony.PropertyKeys.PRIORITY,
                atom.priority.isPresent() ? atom.priority.get().value : null);

        v.property(SemanticSynchrony.PropertyKeys.SOURCE, atom.source.value);
        v.property(SemanticSynchrony.PropertyKeys.TITLE, atom.title);

        setOptionalProperty(v, SemanticSynchrony.PropertyKeys.ALIAS,
                atom.alias.isPresent() ? atom.alias.get() : null);
        setOptionalProperty(v, SemanticSynchrony.PropertyKeys.TEXT,
                atom.text.isPresent() ? atom.text.get() : null);
        setOptionalProperty(v, SemanticSynchrony.PropertyKeys.SHORTCUT,
                atom.shortcut.isPresent() ? atom.shortcut.get() : null);

        // Note: children are managed separately via setChildren()
    }

    // ========== Helper Methods ==========

    private <T> T getRequiredProperty(Vertex v, String key) {
        VertexProperty<T> prop = v.property(key);
        if (!prop.isPresent()) {
            throw new IllegalStateException("Missing required property: " + key + " on vertex " + v.id());
        }
        return prop.value();
    }

    private <T> T getOptionalProperty(Vertex v, String key, T defaultValue) {
        VertexProperty<T> prop = v.property(key);
        return prop.isPresent() ? prop.value() : defaultValue;
    }

    private <T> void setOptionalProperty(Vertex v, String key, T value) {
        if (value == null) {
            VertexProperty<T> prop = v.property(key);
            if (prop.isPresent()) {
                prop.remove();
            }
        } else {
            v.property(key, value);
        }
    }

    private <T> Opt<T> optional(T value) {
        return value == null ? Opt.empty() : Opt.of(value);
    }

    private Opt<Normed> optionalNormed(Float value) {
        return value == null ? Opt.empty() : Opt.of(new Normed(value));
    }

    /**
     * Collect child IDs by navigating the linked list structure.
     * The graph stores children as: parent --NOTES--> listHead --FIRST--> child1
     *                                                           --REST--> next --FIRST--> child2
     */
    private List<AtomId> collectChildIds(Vertex parent) {
        List<AtomId> ids = new ArrayList<>();

        // Get the head of the children list
        var notesEdges = parent.edges(Direction.OUT, SemanticSynchrony.EdgeLabels.NOTES);
        if (!notesEdges.hasNext()) {
            return ids;
        }

        Vertex listNode = notesEdges.next().inVertex();

        // Navigate the linked list
        while (listNode != null) {
            // Get the child via FIRST edge
            var firstEdges = listNode.edges(Direction.OUT, SemanticSynchrony.EdgeLabels.FIRST);
            if (firstEdges.hasNext()) {
                Vertex child = firstEdges.next().inVertex();
                String idValue = getRequiredProperty(child, SemanticSynchrony.PropertyKeys.ID);
                ids.add(new AtomId(idValue));
            }

            // Move to next list node via REST edge
            var restEdges = listNode.edges(Direction.OUT, SemanticSynchrony.EdgeLabels.REST);
            listNode = restEdges.hasNext() ? restEdges.next().inVertex() : null;
        }

        return ids;
    }

    /**
     * Remove all children from a parent atom.
     */
    private void removeAllChildren(Vertex parent) {
        var notesEdges = parent.edges(Direction.OUT, SemanticSynchrony.EdgeLabels.NOTES);
        if (!notesEdges.hasNext()) {
            return;
        }

        Vertex listNode = notesEdges.next().inVertex();
        notesEdges.next().remove(); // Remove NOTES edge

        // Delete all list nodes
        while (listNode != null) {
            var restEdges = listNode.edges(Direction.OUT, SemanticSynchrony.EdgeLabels.REST);
            Vertex next = restEdges.hasNext() ? restEdges.next().inVertex() : null;
            listNode.remove();
            listNode = next;
        }
    }

    /**
     * Create a linked list structure for children.
     */
    private void createChildrenList(Vertex parent, List<AtomId> childIds) {
        if (childIds.isEmpty()) {
            return;
        }

        Vertex prevListNode = null;
        Vertex firstListNode = null;

        for (AtomId childId : childIds) {
            Vertex child = wrapper.getVertexById(childId);
            if (child == null) {
                throw new IllegalArgumentException("Child atom not found: " + childId.value);
            }

            // Create list node
            Vertex listNode = wrapper.getGraph().addVertex();
            listNode.addEdge(SemanticSynchrony.EdgeLabels.FIRST, child);

            if (firstListNode == null) {
                firstListNode = listNode;
            }

            if (prevListNode != null) {
                prevListNode.addEdge(SemanticSynchrony.EdgeLabels.REST, listNode);
            }

            prevListNode = listNode;
        }

        // Connect parent to head of list
        parent.addEdge(SemanticSynchrony.EdgeLabels.NOTES, firstListNode);
    }

    /**
     * Update search indices for a vertex.
     */
    private void updateAllIndices(Vertex v) {
        updateIndex(v, SemanticSynchrony.PropertyKeys.ID);
        updateIndex(v, SemanticSynchrony.PropertyKeys.TITLE);
        updateAcronym(v);
        updateIndex(v, SemanticSynchrony.PropertyKeys.SHORTCUT);
    }

    private void updateIndex(Vertex v, String propertyKey) {
        // TODO: Update Lucene index
        // This will be needed for search functionality
    }

    /**
     * Update the acronym index for a vertex based on its title.
     */
    private void updateAcronym(Vertex v) {
        String title = getOptionalProperty(v, SemanticSynchrony.PropertyKeys.TITLE, null);
        String acronym = valueToAcronym(title);

        setOptionalProperty(v, SemanticSynchrony.PropertyKeys.ACRONYM, acronym);
        if (acronym != null) {
            updateIndex(v, SemanticSynchrony.PropertyKeys.ACRONYM);
        }
    }

    /**
     * Generate an acronym from a title (copied from PGNote).
     */
    private String valueToAcronym(String value) {
        // Index only short, name-like values, avoiding free-form text
        if (value != null && value.length() <= 100) {
            String clean = cleanForAcronym(value);
            StringBuilder acronym = new StringBuilder();
            boolean isInside = false;
            for (byte b : clean.getBytes()) {
                if (b >= 'a' && b <= 'z') {
                    if (!isInside) {
                        acronym.append((char) b);
                        isInside = true;
                    }
                } else if (' ' == b) {
                    isInside = false;
                }
            }
            return acronym.toString();
        }
        return null;
    }

    private String cleanForAcronym(String value) {
        return value.toLowerCase().replaceAll("[-_\t\n\r]", " ").trim();
    }
}
