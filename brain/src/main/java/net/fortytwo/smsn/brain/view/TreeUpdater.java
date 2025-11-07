package net.fortytwo.smsn.brain.view;

import net.fortytwo.smsn.SemanticSynchrony;
import net.fortytwo.smsn.brain.ActivityLog;
import net.fortytwo.smsn.brain.Atom;
import net.fortytwo.smsn.brain.AtomId;
import net.fortytwo.smsn.brain.TreeNode;
import net.fortytwo.smsn.brain.model.Filter;
import net.fortytwo.smsn.brain.repository.AtomRepository;
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

    private final AtomRepository repository;
    private final ActivityLog activityLog;

    public TreeUpdater(AtomRepository repository, ActivityLog activityLog) {
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
        updateProperties(tree, atom);

        // Update children
        updateChildren(tree, atom, height, filter, cache);
    }

    private void updateProperties(TreeNode tree, Atom atom) {
        // Update title (required field in TreeNode)
        String newTitle = tree.title;
        String oldTitle = atom.title;
        if (!newTitle.equals(oldTitle)) {
            repository.updateProperty(atom.id, SemanticSynchrony.PropertyKeys.TITLE, newTitle);
        }

        // Update text if present
        if (tree.text.isPresent()) {
            String newText = tree.text.get();
            String oldText = atom.text.isPresent() ? atom.text.get() : null;
            if (!newText.equals(oldText)) {
                repository.updateProperty(atom.id, SemanticSynchrony.PropertyKeys.TEXT, newText);
            }
        }

        // Update alias if present
        if (tree.alias.isPresent()) {
            String newAlias = tree.alias.get();
            String oldAlias = atom.alias.isPresent() ? atom.alias.get() : null;
            if (!newAlias.equals(oldAlias)) {
                repository.updateProperty(atom.id, SemanticSynchrony.PropertyKeys.ALIAS, newAlias);
            }
        }

        // Update weight (required field)
        float newWeight = tree.weight.value;
        float oldWeight = atom.weight.value;
        if (newWeight != oldWeight) {
            repository.updateProperty(atom.id, SemanticSynchrony.PropertyKeys.WEIGHT, newWeight);
        }

        // Update priority if present
        if (tree.priority.isPresent()) {
            float newPriority = tree.priority.get().value;
            Float oldPriority = atom.priority.isPresent() ? atom.priority.get().value : null;
            if (oldPriority == null || newPriority != oldPriority) {
                repository.updateProperty(atom.id, SemanticSynchrony.PropertyKeys.PRIORITY, newPriority);
            }
        }

        // Update shortcut if present
        if (tree.shortcut.isPresent()) {
            String newShortcut = tree.shortcut.get();
            String oldShortcut = atom.shortcut.isPresent() ? atom.shortcut.get() : null;
            if (!newShortcut.equals(oldShortcut)) {
                repository.updateProperty(atom.id, SemanticSynchrony.PropertyKeys.SHORTCUT, newShortcut);
            }
        }

        // Update source (required field)
        String newSource = tree.source.value;
        String oldSource = atom.source.value;
        if (!newSource.equals(oldSource)) {
            repository.updateProperty(atom.id, SemanticSynchrony.PropertyKeys.SOURCE, newSource);
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

        Atom currentAtom = repository.load(atom.id);
        if (currentAtom == null || (filter != null && !repository.testFilter(currentAtom, filter))) {
            return;
        }

        Set<AtomId> childrenAdded = new HashSet<>();
        Set<AtomId> childrenCreated = new HashSet<>();

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

        // Apply the diff
        final List<AtomId> newChildrenList = new ArrayList<>();

        ListDiff.DiffEditor<TreeNode> editor = new ListDiff.DiffEditor<TreeNode>() {
            @Override
            public void add(int position, TreeNode node) {
                AtomId childId = node.id;
                if (childId == null) {
                    // Create new atom
                    Atom newAtom = repository.createAtom(filter);
                    childId = newAtom.id;
                    childrenCreated.add(childId);
                    cache.put(childId, newAtom);

                    if (activityLog != null) {
                        activityLog.logCreateById(childId);
                    }
                }

                newChildrenList.add(childId);
                childrenAdded.add(childId);

                if (activityLog != null) {
                    activityLog.logLinkById(atom.id, childId);
                }
            }

            @Override
            public void delete(int position, TreeNode node) {
                // Don't add this child to the new list (effectively removes it)
                if (activityLog != null && node.id != null) {
                    activityLog.logUnlinkById(atom.id, node.id);
                }
            }
        };

        ListDiff.applyDiff(currentChildren, desiredChildren, lcs, compareById, editor);

        // Update the atom's children list
        repository.setChildren(atom.id, newChildrenList);

        // Recursively update children
        for (int i = 0; i < desiredChildren.size(); i++) {
            TreeNode childTree = desiredChildren.get(i);
            if (childTree.id == null) {
                // This was a newly created child, use the ID we created
                int addIndex = 0;
                for (AtomId addedId : childrenAdded) {
                    if (addIndex == i) {
                        childTree = childTree.withId(addedId);
                        break;
                    }
                    addIndex++;
                }
            }

            // Determine update depth for this child
            int childHeight;
            if (childrenCreated.contains(childTree.id)) {
                // New atom - only update properties, not grandchildren
                childHeight = 1;
            } else if (childrenAdded.contains(childTree.id)) {
                // Existing atom newly added here - don't update further
                childHeight = 0;
            } else {
                // Existing child - full recursive update
                childHeight = height - 1;
            }

            if (childHeight > 0 && childTree.id != null) {
                updateInternal(childTree, childHeight, filter, cache);
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
            atom = repository.load(id);
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
}
