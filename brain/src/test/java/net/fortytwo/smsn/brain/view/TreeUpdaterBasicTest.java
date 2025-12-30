package net.fortytwo.smsn.brain.view;

import net.fortytwo.smsn.brain.Atom;
import net.fortytwo.smsn.brain.AtomId;
import net.fortytwo.smsn.brain.Brain;
import net.fortytwo.smsn.brain.BrainTestBase;
import net.fortytwo.smsn.brain.TreeNode;
import net.fortytwo.smsn.brain.model.Filter;
import net.fortytwo.smsn.brain.repository.AtomRepository;
import org.junit.Before;
import org.junit.Test;

import java.util.ArrayList;
import java.util.List;

import static org.junit.Assert.*;

/**
 * Critical regression test for TreeUpdater basic functionality.
 * Tests the exact scenario reported: adding a child to a parent should not modify parent's properties.
 */
public class TreeUpdaterBasicTest extends BrainTestBase {
    private AtomRepository repository;
    private TreeUpdater updater;

    @Before
    public void setUp() throws Exception {
        super.setUp();
        repository = brain.getAtomRepository();
        updater = new TreeUpdater(repository, null);  // ActivityLog can be null for testing
    }

    @Test
    public void testAddChildToParentPreservesParentProperties() throws Exception {
        // Create parent atom with specific properties
        AtomId parentId = net.fortytwo.smsn.SemanticSynchrony.createRandomId();
        Atom parent = repository.createAtom(parentId,
            new net.fortytwo.smsn.brain.SourceName("private"),
            "parent atom");

        // Set weight to 0.25 (non-default)
        parent = parent.withWeight(new net.fortytwo.smsn.brain.Normed(0.25f));
        repository.save(parent);

        // Verify initial state
        Atom reloadedParent = repository.load(parentId);
        assertEquals("parent atom", reloadedParent.title);
        assertEquals(0.25f, reloadedParent.weight.value, 0.001f);
        assertEquals("private", reloadedParent.source.value);
        assertEquals(0, reloadedParent.children.size());

        // Create child atom with different properties
        AtomId childId = net.fortytwo.smsn.SemanticSynchrony.createRandomId();
        Atom child = repository.createAtom(childId,
            new net.fortytwo.smsn.brain.SourceName("private"),
            "child atom");

        // Child has default weight 0.5, different from parent's 0.25
        assertEquals(0.5f, child.weight.value, 0.001f);

        // Create tree view with the child
        List<TreeNode> children = new ArrayList<>();
        TreeNode childNode = new TreeNode(
            childId,  // Use existing child's ID
            child.created,
            child.weight,
            child.priority,
            child.source,
            child.title,
            child.alias,
            child.text,
            child.shortcut,
            new ArrayList<>(),
            0,
            0
        );
        children.add(childNode);

        TreeNode parentTree = new TreeNode(
            parentId,
            reloadedParent.created,
            reloadedParent.weight,
            reloadedParent.priority,
            reloadedParent.source,
            reloadedParent.title,
            reloadedParent.alias,
            reloadedParent.text,
            reloadedParent.shortcut,
            children,
            0,
            0
        );

        // Update the tree
        Filter filter = Filter.noFilter();
        updater.update(parentTree, 2, filter);

        // Reload parent and verify properties are UNCHANGED
        Atom updatedParent = repository.load(parentId);

        // CRITICAL: Parent's properties should be the same
        assertEquals("Parent title should be unchanged", "parent atom", updatedParent.title);
        assertEquals("Parent weight should be unchanged", 0.25f, updatedParent.weight.value, 0.001f);
        assertEquals("Parent source should be unchanged", "private", updatedParent.source.value);

        // Parent should now have one child
        assertEquals("Parent should have one child", 1, updatedParent.children.size());

        // Verify the child is correctly linked
        AtomId linkedChildId = updatedParent.children.get(0);
        assertEquals("Linked child should be the one we added", childId, linkedChildId);

        // Load the child and verify it still has its own properties
        Atom linkedChild = repository.load(linkedChildId);
        assertNotNull("Child should exist", linkedChild);
        assertEquals("Child should have correct title", "child atom", linkedChild.title);
        assertEquals("Child should have its own weight", 0.5f, linkedChild.weight.value, 0.001f);

        // Child should have its own properties, not parent's
        assertNotEquals("Child ID should be different from parent", parentId, linkedChildId);
    }

    @Test
    public void testAddChildWithTemporaryIdPreservesParentProperties() throws Exception {
        // This test simulates what happens when JSON is parsed with a child that has no ID.
        // The parser creates a temporary ID like "temp-123" to satisfy TreeNode's non-null requirement.

        // Create parent atom with specific properties
        AtomId parentId = net.fortytwo.smsn.SemanticSynchrony.createRandomId();
        Atom parent = repository.createAtom(parentId,
            new net.fortytwo.smsn.brain.SourceName("private"),
            "parent atom");

        // Set weight to 0.25 (non-default)
        parent = parent.withWeight(new net.fortytwo.smsn.brain.Normed(0.25f));
        repository.save(parent);

        // Verify initial state
        Atom reloadedParent = repository.load(parentId);
        assertEquals("parent atom", reloadedParent.title);
        assertEquals(0.25f, reloadedParent.weight.value, 0.001f);
        assertEquals("private", reloadedParent.source.value);
        assertEquals(0, reloadedParent.children.size());

        // Create tree view with a child that has a temporary ID (simulating JSON parser behavior)
        List<TreeNode> children = new ArrayList<>();
        TreeNode childNode = new TreeNode(
            new net.fortytwo.smsn.brain.AtomId("temp-" + System.nanoTime()),  // Temporary ID
            new net.fortytwo.smsn.brain.Timestamp(System.currentTimeMillis()),
            new net.fortytwo.smsn.brain.Normed(0.5f),
            hydra.util.Opt.empty(),
            new net.fortytwo.smsn.brain.SourceName("private"),
            "child atom",
            hydra.util.Opt.empty(),
            hydra.util.Opt.empty(),
            hydra.util.Opt.empty(),
            new ArrayList<>(),
            0,
            0
        );
        children.add(childNode);

        TreeNode parentTree = new TreeNode(
            parentId,
            reloadedParent.created,
            reloadedParent.weight,
            reloadedParent.priority,
            reloadedParent.source,
            reloadedParent.title,
            reloadedParent.alias,
            reloadedParent.text,
            reloadedParent.shortcut,
            children,
            0,
            0
        );

        // Update the tree
        Filter filter = Filter.noFilter();
        updater.update(parentTree, 2, filter);

        // Reload parent and verify properties are UNCHANGED
        Atom updatedParent = repository.load(parentId);

        // CRITICAL: Parent's properties should be the same
        assertEquals("Parent title should be unchanged", "parent atom", updatedParent.title);
        assertEquals("Parent weight should be unchanged", 0.25f, updatedParent.weight.value, 0.001f);
        assertEquals("Parent source should be unchanged", "private", updatedParent.source.value);

        // Parent should now have one child
        assertEquals("Parent should have one child", 1, updatedParent.children.size());

        // Load the child and verify it exists with correct properties
        AtomId childId = updatedParent.children.get(0);
        Atom child = repository.load(childId);
        assertNotNull("Child should exist", child);
        assertEquals("Child should have correct title", "child atom", child.title);
        assertEquals("Child should have default weight", 0.5f, child.weight.value, 0.001f);

        // Child should have a real ID, not the temporary one
        assertFalse("Child ID should not be temporary", childId.value.startsWith("temp-"));
        assertNotEquals("Child ID should be different from parent", parentId, childId);
    }
}
