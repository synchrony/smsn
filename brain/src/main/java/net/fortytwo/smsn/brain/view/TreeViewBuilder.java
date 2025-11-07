package net.fortytwo.smsn.brain.view;

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
     * Build a tree view starting from a root atom.
     *
     * @param rootId the root atom ID
     * @param height maximum tree depth (0 = root only, 1 = root + children, etc.)
     * @param filter filter for which atoms to include
     * @return immutable TreeNode with children populated up to specified height
     */
    public TreeNode buildView(AtomId rootId, int height, Filter filter) {
        Set<AtomId> visited = new HashSet<>();
        return buildViewInternal(rootId, height, filter, visited);
    }

    /**
     * Build tree view from a list of atoms (e.g., search results).
     * Creates a virtual root with the atoms as children.
     *
     * @param atoms list of atoms to include as top-level results
     * @param childHeight height to expand each result's children
     * @param filter filter for child atoms
     * @return TreeNode representing the result set
     */
    public TreeNode buildSearchResultsView(List<Atom> atoms, int childHeight, Filter filter) {
        List<TreeNode> childNodes = new ArrayList<>();
        Set<AtomId> visited = new HashSet<>();

        for (Atom atom : atoms) {
            if (!visited.contains(atom.id)) {
                childNodes.add(buildViewInternal(atom.id, childHeight, filter, visited));
            }
        }

        // Create a virtual root node for search results
        // Use the first atom's properties as template, but with special markers
        if (atoms.isEmpty()) {
            // Return empty result set
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
    private TreeNode buildViewInternal(AtomId atomId, int remainingHeight, Filter filter, Set<AtomId> visited) {
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

        // Base case: no more height, don't expand children
        if (remainingHeight <= 0) {
            return atomToTreeNode(atom, new ArrayList<>());
        }

        // Recursive case: expand children
        List<TreeNode> childNodes = new ArrayList<>();
        for (AtomId childId : atom.children) {
            TreeNode childNode = buildViewInternal(childId, remainingHeight - 1, filter, new HashSet<>(visited));
            if (childNode != null) {
                childNodes.add(childNode);
            }
        }

        return atomToTreeNode(atom, childNodes);
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
}
