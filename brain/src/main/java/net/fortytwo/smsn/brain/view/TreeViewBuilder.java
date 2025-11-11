package net.fortytwo.smsn.brain.view;

import net.fortytwo.smsn.SemanticSynchrony;
import net.fortytwo.smsn.brain.Atom;
import net.fortytwo.smsn.brain.AtomId;
import net.fortytwo.smsn.brain.TreeNode;
import net.fortytwo.smsn.brain.model.Filter;
import net.fortytwo.smsn.brain.repository.AtomRepository;

import java.util.ArrayList;
import java.util.HashSet;
import java.util.List;
import java.util.Set;

/**
 * Builds hierarchical tree views from flat Atom structures.
 * Uses only generated immutable classes (Atom and TreeNode), with no dependency on old Note/Link classes.
 */
public class TreeViewBuilder {

    private final AtomRepository repository;

    public TreeViewBuilder(AtomRepository repository) {
        this.repository = repository;
    }

    /**
     * Build a forward tree view starting from a root atom (expanding children).
     *
     * @param rootId the root atom ID
     * @param height maximum tree depth (0 = root only, 1 = root + children, etc.)
     * @param filter filter for which atoms to include
     * @return immutable TreeNode with children populated up to specified height
     */
    public TreeNode buildView(AtomId rootId, int height, Filter filter) {
        return buildView(rootId, height, filter, ViewDirection.FORWARD);
    }

    /**
     * Build a tree view starting from a root atom with specified direction.
     *
     * @param rootId the root atom ID
     * @param height maximum tree depth (0 = root only, 1 = root + linked nodes, etc.)
     * @param filter filter for which atoms to include
     * @param direction FORWARD to expand children, BACKWARD to expand parents
     * @return immutable TreeNode with linked nodes populated up to specified height
     */
    public TreeNode buildView(AtomId rootId, int height, Filter filter, ViewDirection direction) {
        Set<AtomId> visited = new HashSet<>();
        return buildViewInternal(rootId, height, filter, visited, direction);
    }

    /**
     * Build a simple list view from atoms with no children expanded.
     * Creates a virtual root with the atoms as direct children (height 0).
     *
     * @param atoms list of atoms to include
     * @param filter filter for atoms
     * @return TreeNode representing the flat list
     */
    public TreeNode buildListView(List<Atom> atoms, Filter filter) {
        List<TreeNode> childNodes = new ArrayList<>();

        for (Atom atom : atoms) {
            if (filter == null || passesFilter(atom, filter)) {
                // Add each atom with no children (height 0)
                childNodes.add(atomToTreeNode(atom, new ArrayList<>()));
            }
        }

        // Create a virtual root for the list
        if (atoms.isEmpty()) {
            Atom firstAtom = repository.getAllAtomIds().stream()
                    .findFirst()
                    .map(repository::load)
                    .orElse(null);
            if (firstAtom == null) {
                throw new IllegalStateException("No atoms in repository");
            }
            return atomToTreeNode(firstAtom, new ArrayList<>());
        }

        Atom firstAtom = atoms.get(0);
        return new TreeNode(
                new AtomId("list-view"),
                firstAtom.created,
                firstAtom.weight,
                firstAtom.priority,
                firstAtom.source,
                "List View",
                firstAtom.alias,
                firstAtom.text,
                firstAtom.shortcut,
                childNodes,
                childNodes.size(),
                0
        );
    }

    /**
     * Build tree view from a list of atoms (e.g., search results) with forward direction.
     *
     * @param atoms list of atoms to include as top-level results
     * @param childHeight height to expand each result's children
     * @param filter filter for child atoms
     * @return TreeNode representing the result set
     */
    public TreeNode buildSearchResultsView(List<Atom> atoms, int childHeight, Filter filter) {
        return buildSearchResultsView(atoms, childHeight, filter, ViewDirection.FORWARD);
    }

    /**
     * Build tree view from a list of atoms (e.g., search results).
     * Creates a virtual root with the atoms as children.
     *
     * @param atoms list of atoms to include as top-level results
     * @param childHeight height to expand each result's linked nodes
     * @param filter filter for linked atoms
     * @param direction FORWARD to expand children, BACKWARD to expand parents
     * @return TreeNode representing the result set
     */
    public TreeNode buildSearchResultsView(List<Atom> atoms, int childHeight, Filter filter, ViewDirection direction) {
        List<TreeNode> childNodes = new ArrayList<>();
        Set<AtomId> visited = new HashSet<>();

        // Use childHeight - 1 to match legacy behavior where search results showed
        // only the matched atoms without expanding children at height 1
        int actualHeight = childHeight > 0 ? childHeight - 1 : 0;

        for (Atom atom : atoms) {
            if (!visited.contains(atom.id)) {
                childNodes.add(buildViewInternal(atom.id, actualHeight, filter, visited, direction));
            }
        }

        // Create a virtual root node for search results
        if (atoms.isEmpty()) {
            // Return empty result set with default properties
            return new TreeNode(
                    new AtomId("search-results"),
                    new net.fortytwo.smsn.brain.Timestamp(System.currentTimeMillis()),
                    new net.fortytwo.smsn.brain.Normed(SemanticSynchrony.DEFAULT_WEIGHT),
                    hydra.util.Opt.empty(),
                    new net.fortytwo.smsn.brain.SourceName("public"),
                    "Search Results (no matches)",
                    hydra.util.Opt.empty(),
                    hydra.util.Opt.empty(),
                    hydra.util.Opt.empty(),
                    new ArrayList<>(),  // Empty children
                    0,
                    0
            );
        }

        // Use the first atom's properties as template for the virtual root
        Atom firstAtom = atoms.get(0);
        return new TreeNode(
                new AtomId("search-results"),
                firstAtom.created,
                firstAtom.weight,
                firstAtom.priority,
                firstAtom.source,
                "Search Results",
                firstAtom.alias,
                firstAtom.text,
                firstAtom.shortcut,
                childNodes,
                childNodes.size(),
                0
        );
    }

    /**
     * Internal recursive builder with cycle detection.
     */
    private TreeNode buildViewInternal(AtomId atomId, int remainingHeight, Filter filter, Set<AtomId> visited, ViewDirection direction) {
        // Prevent cycles
        if (visited.contains(atomId)) {
            // Return stub node for cycle detection
            Atom atom = repository.load(atomId);
            return atomToTreeNode(atom, new ArrayList<>());
        }

        visited.add(atomId);

        // Load the atom
        Atom atom = repository.load(atomId);

        // Apply filter
        if (filter != null && !passesFilter(atom, filter)) {
            return null;
        }

        // Base case: no more height, don't expand linked nodes
        if (remainingHeight <= 0) {
            return atomToTreeNode(atom, new ArrayList<>());
        }

        // Recursive case: expand linked nodes based on direction
        List<TreeNode> linkedNodes = new ArrayList<>();
        List<AtomId> linkedIds = direction == ViewDirection.FORWARD
            ? atom.children
            : repository.getParentIds(atomId);

        for (AtomId linkedId : linkedIds) {
            TreeNode linkedNode = buildViewInternal(linkedId, remainingHeight - 1, filter, new HashSet<>(visited), direction);
            if (linkedNode != null) {
                linkedNodes.add(linkedNode);
            }
        }

        return atomToTreeNode(atom, linkedNodes);
    }

    /**
     * Convert an Atom to a TreeNode with specified children.
     */
    private TreeNode atomToTreeNode(Atom atom, List<TreeNode> children) {
        // Get actual counts from repository
        int totalChildren = atom.children.size();
        int parents = repository.countParents(atom.id);

        return new TreeNode(
                atom.id,
                atom.created,
                atom.weight,
                atom.priority,
                atom.source,
                atom.title,
                atom.alias,
                atom.text,
                atom.shortcut,
                children,
                totalChildren,
                parents
        );
    }

    /**
     * Check if an atom passes the filter criteria.
     */
    private boolean passesFilter(Atom atom, Filter filter) {
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
            // Get source indices from configuration
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
        List<net.fortytwo.smsn.config.DataSource> sources =
                net.fortytwo.smsn.SemanticSynchrony.getConfiguration().getSources();
        for (int i = 0; i < sources.size(); i++) {
            if (sources.get(i).getName().equals(sourceName)) {
                return i;
            }
        }
        return null;
    }

    /**
     * Create a simple TreeNode with no optional fields.
     * Helper for code outside brain module that cannot directly access Opt.
     */
    public static TreeNode createSimpleTreeNode(
            AtomId id,
            net.fortytwo.smsn.brain.Timestamp created,
            net.fortytwo.smsn.brain.Normed weight,
            net.fortytwo.smsn.brain.SourceName source,
            String title,
            List<TreeNode> children,
            int totalChildren,
            int numberOfParents) {
        return new TreeNode(
                id,
                created,
                weight,
                hydra.util.Opt.empty(),  // no priority
                source,
                title,
                hydra.util.Opt.empty(),  // no alias
                hydra.util.Opt.empty(),  // no text
                hydra.util.Opt.empty(),  // no shortcut
                children,
                totalChildren,
                numberOfParents
        );
    }

    /**
     * Convert an Atom to a TreeNode with specified children.
     * Helper for code outside brain module that cannot directly access Opt.
     */
    public static TreeNode atomToTreeNode(Atom atom, List<TreeNode> children, int numberOfParents) {
        return new TreeNode(
                atom.id,
                atom.created,
                atom.weight,
                atom.priority,
                atom.source,
                atom.title,
                atom.alias,
                atom.text,
                atom.shortcut,
                children,
                atom.children.size(),
                numberOfParents
        );
    }

    /**
     * Create an Atom with basic properties and optional text.
     * Helper for code outside brain module that cannot directly access Opt.
     */
    public static Atom createAtom(
            AtomId id,
            long createdTimestamp,
            float weight,
            String source,
            String title,
            String text,
            List<AtomId> children) {
        return new Atom(
                id,
                new net.fortytwo.smsn.brain.Timestamp(createdTimestamp),
                new net.fortytwo.smsn.brain.Normed(weight),
                hydra.util.Opt.empty(), // priority
                new net.fortytwo.smsn.brain.SourceName(source),
                title,
                hydra.util.Opt.empty(), // alias
                text != null ? hydra.util.Opt.of(text) : hydra.util.Opt.empty(),
                hydra.util.Opt.empty(), // shortcut
                children
        );
    }
}
