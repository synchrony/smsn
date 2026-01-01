package net.fortytwo.smsn.brain.repository;

import net.fortytwo.smsn.SemanticSynchrony;
import net.fortytwo.smsn.brain.Atom;
import net.fortytwo.smsn.brain.AtomId;
import net.fortytwo.smsn.brain.TreeNode;
import net.fortytwo.smsn.brain.model.Filter;
import net.fortytwo.smsn.config.DataSource;

import java.util.Collections;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.stream.Collectors;

/**
 * Builder for constructing hierarchical TreeNode views from Atoms.
 * Handles recursive tree construction with filtering and depth control.
 */
public class TreeViewBuilder {
    private final AtomRepositoryInterface repository;
    private static final Map<String, Integer> sourceToIndex = buildSourceIndex();

    public TreeViewBuilder(AtomRepositoryInterface repository) {
        this.repository = repository;
    }

    private static Map<String, Integer> buildSourceIndex() {
        Map<String, Integer> map = new HashMap<>();
        List<DataSource> sources = SemanticSynchrony.getConfiguration().getSources();
        for (int i = 0; i < sources.size(); i++) {
            map.put(sources.get(i).getName(), i);
        }
        return map;
    }

    /**
     * Build a tree view starting from a root atom.
     *
     * @param rootId The ID of the root atom
     * @param height Maximum depth of the tree (0 = just root, 1 = root + children, etc.)
     * @param filter Filter to apply to atoms (null = no filtering)
     * @return A TreeNode representing the hierarchical view
     */
    public TreeNode buildView(AtomId rootId, int height, Filter filter) {
        Atom rootAtom = repository.load(rootId);

        if (filter != null && !testAtom(rootAtom, filter)) {
            // If root doesn't pass filter, return empty node
            return createEmptyTreeNode(rootAtom);
        }

        return buildTreeNode(rootAtom, height, filter);
    }

    /**
     * Build a custom view from a list of atom IDs.
     *
     * @param atomIds List of atom IDs to include
     * @param filter Filter to apply
     * @return A TreeNode containing all specified atoms as children
     */
    public TreeNode buildCustomView(List<AtomId> atomIds, Filter filter) {
        List<TreeNode> children = atomIds.stream()
                .map(repository::load)
                .filter(atom -> filter == null || testAtom(atom, filter))
                .map(atom -> buildTreeNode(atom, 0, filter))
                .collect(Collectors.toList());

        // Create a virtual root node
        return createVirtualRoot(children);
    }

    /**
     * Build a view from search results.
     *
     * @param results List of atoms from search
     * @param height Height of each result subtree
     * @param filter Filter to apply
     * @return A TreeNode containing all results as children
     */
    public TreeNode buildSearchResults(List<Atom> results, int height, Filter filter) {
        List<TreeNode> children = results.stream()
                .filter(atom -> filter == null || testAtom(atom, filter))
                .map(atom -> buildTreeNode(atom, height, filter))
                .collect(Collectors.toList());

        return createVirtualRoot(children);
    }

    // ========== Private Methods ==========

    /**
     * Recursively build a TreeNode from an Atom.
     */
    private TreeNode buildTreeNode(Atom atom, int depth, Filter filter) {
        // Build children if we haven't reached max depth
        List<TreeNode> children;
        if (depth > 0) {
            children = atom.children.stream()
                    .map(repository::load)
                    .filter(child -> filter == null || testAtom(child, filter))
                    .map(child -> buildTreeNode(child, depth - 1, filter))
                    .collect(Collectors.toList());
        } else {
            children = Collections.emptyList();
        }

        // Count parents for this atom
        int numberOfParents = repository.countParents(atom.id);

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
                atom.children.size(),  // numberOfChildren (from atom's child list)
                numberOfParents
        );
    }

    /**
     * Test if an Atom passes the filter criteria.
     * This replicates the logic from Filter.test(Note) but works with Atoms.
     */
    private boolean testAtom(Atom atom, Filter filter) {
        if (filter == null || filter.isTrivial()) {
            return true;
        }

        // Get weight
        float weight = atom.weight.value;
        float minWeight = filter.getMinWeight();

        if (weight < minWeight) {
            return false;
        }

        // Check source - empty includedSources means include all
        java.util.Set<String> includedSources = filter.getIncludedSources();
        if (includedSources != null && !includedSources.isEmpty()) {
            if (!includedSources.contains(atom.source.value)) {
                return false;
            }
        }

        return true;
    }

    /**
     * Create an empty TreeNode (used when atom doesn't pass filter).
     */
    private TreeNode createEmptyTreeNode(Atom atom) {
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
                Collections.emptyList(),  // no children
                0,  // numberOfChildren
                0   // numberOfParents
        );
    }

    /**
     * Create a virtual root node for collections of results.
     */
    private TreeNode createVirtualRoot(List<TreeNode> children) {
        // Create a placeholder root with minimal info
        return new TreeNode(
                new AtomId("virtual-root"),
                new net.fortytwo.smsn.brain.Timestamp(System.currentTimeMillis()),
                new net.fortytwo.smsn.brain.Normed(0.5f),
                hydra.util.Opt.empty(),
                new net.fortytwo.smsn.brain.SourceName("public"),
                "Search Results",
                hydra.util.Opt.empty(),
                hydra.util.Opt.empty(),
                hydra.util.Opt.empty(),
                children,
                children.size(),
                0
        );
    }
}
