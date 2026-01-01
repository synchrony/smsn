package net.fortytwo.smsn.brain.view;

import net.fortytwo.smsn.SemanticSynchrony;
import net.fortytwo.smsn.brain.ActivityLog;
import net.fortytwo.smsn.brain.Atom;
import net.fortytwo.smsn.brain.AtomId;
import net.fortytwo.smsn.brain.TreeNode;
import net.fortytwo.smsn.brain.model.Filter;
import net.fortytwo.smsn.brain.repository.AtomRepositoryInterface;
import net.fortytwo.smsn.brain.util.ListDiff;

import java.util.ArrayList;
import java.util.Comparator;
import java.util.HashMap;
import java.util.HashSet;
import java.util.List;
import java.util.Map;
import java.util.Set;

/**
 * Service for updating the graph based on tree structures.
 * This replaces the old TreeViews.update() method with a version that works with Atoms.
 */
public class TreeUpdater {

    private final AtomRepositoryInterface repository;
    private final ActivityLog activityLog;

    public TreeUpdater(AtomRepositoryInterface repository, ActivityLog activityLog) {
        this.repository = repository;
        this.activityLog = activityLog;
    }

    /**
     * Update the graph to match the given tree structure
     *
     * @param tree The tree structure to apply
     * @param height Maximum depth to update
     * @param filter Filter for determining which atoms to process
     */
    public void update(TreeNode tree, int height, Filter filter) {
        if (tree.id == null) {
            throw new IllegalArgumentException("Tree root must have an ID");
        }

        Map<AtomId, Atom> cache = new HashMap<>();
        updateInternal(tree, height, filter, cache);

        // Notify that the graph has been updated
        repository.notifyOfUpdate();
    }

    private void updateInternal(TreeNode tree, int height, Filter filter, Map<AtomId, Atom> cache) {
        Atom atom = getOrCreateAtom(tree.id, filter, cache);

        // Update properties of this atom
        updateProperties(tree, atom, cache);

        // Update children
        updateChildren(tree, atom, height, filter, cache);
    }

    private void updateProperties(TreeNode tree, Atom atom, Map<AtomId, Atom> cache) {
        boolean updated = false;

        // Always update title (even if empty, it's explicitly set)
        String newTitle = tree.title;
        String oldTitle = atom.title;
        if (!newTitle.equals(oldTitle)) {
            repository.updateProperty(atom.id, SemanticSynchrony.PropertyKeys.TITLE, newTitle);
            updated = true;
        }

        // Update optional properties if explicitly present in the tree
        // WikiParser now parses @alias, @text, @weight, etc. from wiki text
        // Empty string values (e.g., "@alias" alone) mean "clear this property"
        if (tree.alias.isPresent()) {
            String newAlias = tree.alias.get();
            String oldAlias = atom.alias.isPresent() ? atom.alias.get() : null;
            // Empty string means "clear the property" - pass null to repository
            Object valueToSet = newAlias.isEmpty() ? null : newAlias;
            if (!java.util.Objects.equals(newAlias.isEmpty() ? null : newAlias, oldAlias)) {
                repository.updateProperty(atom.id, SemanticSynchrony.PropertyKeys.ALIAS, valueToSet);
                updated = true;
            }
        }

        if (tree.text.isPresent()) {
            String newText = tree.text.get();
            String oldText = atom.text.isPresent() ? atom.text.get() : null;
            // Empty string means "clear the property" - pass null to repository
            Object valueToSet = newText.isEmpty() ? null : newText;
            if (!java.util.Objects.equals(newText.isEmpty() ? null : newText, oldText)) {
                repository.updateProperty(atom.id, SemanticSynchrony.PropertyKeys.TEXT, valueToSet);
                updated = true;
            }
        }

        if (tree.shortcut.isPresent()) {
            String newShortcut = tree.shortcut.get();
            String oldShortcut = atom.shortcut.isPresent() ? atom.shortcut.get() : null;
            // Empty string means "clear the property" - pass null to repository
            Object valueToSet = newShortcut.isEmpty() ? null : newShortcut;
            if (!java.util.Objects.equals(newShortcut.isEmpty() ? null : newShortcut, oldShortcut)) {
                repository.updateProperty(atom.id, SemanticSynchrony.PropertyKeys.SHORTCUT, valueToSet);
                updated = true;
            }
        }

        if (tree.priority.isPresent()) {
            float newPriority = tree.priority.get().value;
            Float oldPriority = atom.priority.isPresent() ? atom.priority.get().value : null;
            // Sentinel value -1.0f means "clear the priority" (from empty "@priority")
            if (newPriority < 0.0f) {
                // Clear the priority if it exists
                if (oldPriority != null) {
                    repository.updateProperty(atom.id, SemanticSynchrony.PropertyKeys.PRIORITY, null);
                    updated = true;
                }
            } else if (oldPriority == null || newPriority != oldPriority) {
                repository.updateProperty(atom.id, SemanticSynchrony.PropertyKeys.PRIORITY, newPriority);
                updated = true;
            }
        }

        // Update weight if explicitly provided in the tree
        // Note: Weight is always present in TreeNode (non-optional), so we check if it differs from current
        float newWeight = tree.weight.value;
        float oldWeight = atom.weight.value;
        if (newWeight != oldWeight) {
            repository.updateProperty(atom.id, SemanticSynchrony.PropertyKeys.WEIGHT, newWeight);
            updated = true;
        }

        // Note: We don't update source here because:
        // - Source should typically not change for existing atoms
        // - Changing source could have security implications

        // Invalidate cache after updates so subsequent accesses get fresh atom
        if (updated) {
            cache.remove(atom.id);
        }

        // Log the property update
        if (activityLog != null) {
            activityLog.logSetPropertiesById(atom.id);
        }
    }

    private void updateChildren(TreeNode tree, Atom atom, int height, Filter filter, Map<AtomId, Atom> cache) {
        if (height <= 0) {
            return;
        }

        Atom currentAtom = repository.findById(atom.id).orElse(null);
        if (currentAtom == null || (filter != null && !repository.testFilter(currentAtom, filter))) {
            return;
        }

        Set<AtomId> childrenAdded = new HashSet<>();
        Set<AtomId> childrenCreated = new HashSet<>();
        Map<Integer, AtomId> positionToCreatedId = new HashMap<>();

        // Get current children from the graph
        List<TreeNode> currentChildren = new ArrayList<>();
        for (AtomId childId : currentAtom.children) {
            Atom childAtom = getAtom(childId, cache);
            if (childAtom != null && (filter == null || repository.testFilter(childAtom, filter))) {
                currentChildren.add(convertAtomToTreeNode(childAtom));
            }
        }

        // Get desired children from the tree
        List<TreeNode> desiredChildren = tree.children;

        // Compute diff using LCS
        Comparator<TreeNode> compareById = (a, b) -> {
            AtomId idA = a.id;
            AtomId idB = b.id;
            if (idA == null && idB == null) return 0;
            if (idA == null) return -1;
            if (idB == null) return 1;
            return idA.value.compareTo(idB.value);
        };

        List<TreeNode> lcs = ListDiff.longestCommonSubsequence(currentChildren, desiredChildren, compareById);

        // Apply the diff using in-place modifications
        ListDiff.DiffEditor<TreeNode> editor = new ListDiff.DiffEditor<TreeNode>() {
            @Override
            public void add(int position, TreeNode node) {
                AtomId childId = node.id;
                // Check if we need to create a new atom:
                // - childId is null, OR
                // - childId is a temporary ID (starts with "temp-"), OR
                // - atom doesn't exist in the repository
                boolean needsCreation = childId == null
                    || childId.value.startsWith("temp-")
                    || getAtom(childId, cache) == null;

                if (needsCreation) {
                    // Create new atom
                    Atom newAtom = repository.createAtom(filter);
                    childId = newAtom.id;
                    childrenCreated.add(childId);
                    positionToCreatedId.put(position, childId);  // Track position -> ID mapping
                    cache.put(childId, newAtom);

                    if (activityLog != null) {
                        activityLog.logCreateById(childId);
                    }
                }

                childrenAdded.add(childId);

                // Reload atom to get current children list (after previous adds/deletes)
                Atom freshAtom = repository.findById(atom.id).orElse(null);
                if (freshAtom == null) {
                    throw new IllegalStateException("Parent atom disappeared: " + atom.id.value);
                }

                // Convert visible position to actual position in unfiltered list
                int actualPosition = indexOfNthVisible(freshAtom.children, position, filter, cache);
                repository.addChildAt(atom.id, childId, actualPosition);

                if (activityLog != null) {
                    activityLog.logLinkById(atom.id, childId);
                }
            }

            @Override
            public void delete(int position, TreeNode node) {
                // Reload atom to get current children list (after previous adds/deletes)
                Atom freshAtom = repository.findById(atom.id).orElse(null);
                if (freshAtom == null) {
                    throw new IllegalStateException("Parent atom disappeared: " + atom.id.value);
                }

                // Convert visible position to actual position in unfiltered list
                int actualPosition = indexOfNthVisible(freshAtom.children, position, filter, cache);
                repository.deleteChildAt(atom.id, actualPosition);

                if (activityLog != null && node.id != null) {
                    activityLog.logUnlinkById(atom.id, node.id);
                }
            }

            @Override
            public void retain(int position, TreeNode node) {
                // Child is in both current and desired - no action needed
            }
        };

        ListDiff.applyDiff(currentChildren, desiredChildren, lcs, compareById, editor);

        // Recursively update children
        for (int i = 0; i < desiredChildren.size(); i++) {
            TreeNode childTree = desiredChildren.get(i);
            // Check if this child was newly created (null ID, temp ID, or in the created set)
            if (childTree.id == null || childTree.id.value.startsWith("temp-")) {
                // This was a newly created child, look up the ID we created for this position
                AtomId createdId = positionToCreatedId.get(i);
                if (createdId != null) {
                    childTree = childTree.withId(createdId);
                } else {
                    // No ID found - this shouldn't happen, skip this child
                    continue;
                }
            }

            // Determine whether and how to update this child
            if (childTree.id == null) {
                continue;  // Skip if no ID
            }

            if (childrenCreated.contains(childTree.id)) {
                // New atom - update properties only, not grandchildren
                updateInternal(childTree, 1, filter, cache);
            } else if (childrenAdded.contains(childTree.id)) {
                // Existing atom newly linked here - don't update its properties
                // (we only added a link to it; the atom's content should not change)
                // Skip the update
            } else {
                // Existing child already linked - full recursive update
                // Always recurse, even if height-1 = 0, to update properties at this level
                updateInternal(childTree, height - 1, filter, cache);
            }
        }
    }

    private Atom getOrCreateAtom(AtomId id, Filter filter, Map<AtomId, Atom> cache) {
        Atom atom = getAtom(id, cache);
        if (atom == null) {
            atom = repository.createAtom(filter);
            cache.put(atom.id, atom);

            if (activityLog != null) {
                activityLog.logCreateById(atom.id);
            }
        }
        return atom;
    }

    private Atom getAtom(AtomId id, Map<AtomId, Atom> cache) {
        if (id == null) {
            return null;
        }

        Atom atom = cache.get(id);
        if (atom == null) {
            // Use findById instead of load to avoid exception when atom doesn't exist
            atom = repository.findById(id).orElse(null);
            if (atom != null) {
                cache.put(id, atom);
            }
        }
        return atom;
    }

    private TreeNode convertAtomToTreeNode(Atom atom) {
        // Convert atom to minimal TreeNode for diff comparison
        // TreeNode constructor: id, created, weight, priority, source, title, alias, text, shortcut, children, numberOfChildren, numberOfParents
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
            new ArrayList<>(),  // Don't include children for diff purposes
            0,  // numberOfChildren
            0   // numberOfParents
        );
    }

    /**
     * Convert a visible (filtered) position to an actual position in the unfiltered list.
     * This accounts for children that are filtered out.
     *
     * @param allChildren the complete list of children (unfiltered)
     * @param visiblePosition the position among visible (filtered) children
     * @param filter the filter to apply
     * @param cache cache of loaded atoms
     * @return the actual position in the unfiltered list
     */
    private int indexOfNthVisible(List<AtomId> allChildren, int visiblePosition, Filter filter, Map<AtomId, Atom> cache) {
        int visibleCount = 0;
        for (int i = 0; i < allChildren.size(); i++) {
            AtomId childId = allChildren.get(i);
            Atom childAtom = getAtom(childId, cache);

            if (childAtom != null && (filter == null || repository.testFilter(childAtom, filter))) {
                if (visibleCount == visiblePosition) {
                    return i;
                }
                visibleCount++;
            }
        }

        // If we get here, position is at the end (for adds)
        return allChildren.size();
    }
}
