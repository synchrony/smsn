package net.fortytwo.smsn.brain.view;

import net.fortytwo.smsn.brain.Atom;
import net.fortytwo.smsn.brain.AtomId;
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
 * Tests for updating grandchildren properties via TreeUpdater.
 * Regression test for bug where grandchildren properties weren't updated.
 */
public class TreeUpdaterGrandchildTest extends BrainTestBase {
    private AtomRepository repository;
    private TreeUpdater updater;

    @Before
    public void setUp() throws Exception {
        super.setUp();
        repository = brain.getAtomRepository();
        updater = new TreeUpdater(repository, null);
    }

    @Test
    public void testUpdateGrandchildTitle() throws Exception {
        // Create root -> child -> grandchild hierarchy
        AtomId rootId = net.fortytwo.smsn.SemanticSynchrony.createRandomId();
        AtomId childId = net.fortytwo.smsn.SemanticSynchrony.createRandomId();
        AtomId grandchildId = net.fortytwo.smsn.SemanticSynchrony.createRandomId();

        Atom root = repository.createAtom(rootId,
            new net.fortytwo.smsn.brain.SourceName("public"),
            "root");
        Atom child = repository.createAtom(childId,
            new net.fortytwo.smsn.brain.SourceName("public"),
            "child");
        Atom grandchild = repository.createAtom(grandchildId,
            new net.fortytwo.smsn.brain.SourceName("public"),
            "grandchild");

        // Link: root -> child -> grandchild
        repository.addChildAt(rootId, childId, 0);
        repository.addChildAt(childId, grandchildId, 0);

        // Verify initial state
        Atom reloadedGrandchild = repository.load(grandchildId);
        assertEquals("Initial grandchild title", "grandchild", reloadedGrandchild.title);

        // Create tree update with modified grandchild title
        TreeNode grandchildNode = new TreeNode(
            grandchildId,
            grandchild.created,
            grandchild.weight,
            grandchild.priority,
            grandchild.source,
            "grandchild updated",  // Changed title
            grandchild.alias,
            grandchild.text,
            grandchild.shortcut,
            new ArrayList<>(),
            0,
            0
        );

        TreeNode childNode = new TreeNode(
            childId,
            child.created,
            child.weight,
            child.priority,
            child.source,
            "child",
            child.alias,
            child.text,
            child.shortcut,
            List.of(grandchildNode),
            1,
            0
        );

        TreeNode rootNode = new TreeNode(
            rootId,
            root.created,
            root.weight,
            root.priority,
            root.source,
            "root",
            root.alias,
            root.text,
            root.shortcut,
            List.of(childNode),
            1,
            0
        );

        // Update with height=2 (to reach grandchildren)
        updater.update(rootNode, 2, Filter.noFilter());

        // Verify grandchild title was updated
        Atom updatedGrandchild = repository.load(grandchildId);
        assertEquals("Grandchild title should be updated", "grandchild updated", updatedGrandchild.title);
    }

    @Test
    public void testUpdateGrandchildProperties() throws Exception {
        // Create root -> child -> grandchild hierarchy
        AtomId rootId = net.fortytwo.smsn.SemanticSynchrony.createRandomId();
        AtomId childId = net.fortytwo.smsn.SemanticSynchrony.createRandomId();
        AtomId grandchildId = net.fortytwo.smsn.SemanticSynchrony.createRandomId();

        Atom root = repository.createAtom(rootId,
            new net.fortytwo.smsn.brain.SourceName("public"),
            "root");
        Atom child = repository.createAtom(childId,
            new net.fortytwo.smsn.brain.SourceName("public"),
            "child");
        Atom grandchild = repository.createAtom(grandchildId,
            new net.fortytwo.smsn.brain.SourceName("public"),
            "grandchild");

        // Link: root -> child -> grandchild
        repository.addChildAt(rootId, childId, 0);
        repository.addChildAt(childId, grandchildId, 0);

        // Create tree update with modified grandchild properties
        TreeNode grandchildNode = new TreeNode(
            grandchildId,
            grandchild.created,
            new net.fortytwo.smsn.brain.Normed(0.75f),  // Changed weight
            hydra.util.Opt.of(new net.fortytwo.smsn.brain.Normed(0.9f)),  // Added priority
            grandchild.source,
            "grandchild with properties",  // Changed title
            hydra.util.Opt.of("http://example.org"),  // Added alias
            grandchild.text,
            grandchild.shortcut,
            new ArrayList<>(),
            0,
            0
        );

        TreeNode childNode = new TreeNode(
            childId,
            child.created,
            child.weight,
            child.priority,
            child.source,
            "child",
            child.alias,
            child.text,
            child.shortcut,
            List.of(grandchildNode),
            1,
            0
        );

        TreeNode rootNode = new TreeNode(
            rootId,
            root.created,
            root.weight,
            root.priority,
            root.source,
            "root",
            root.alias,
            root.text,
            root.shortcut,
            List.of(childNode),
            1,
            0
        );

        // Update with height=2 (to reach grandchildren)
        updater.update(rootNode, 2, Filter.noFilter());

        // Verify grandchild properties were updated
        Atom updatedGrandchild = repository.load(grandchildId);
        assertEquals("Grandchild title should be updated",
            "grandchild with properties", updatedGrandchild.title);
        assertEquals("Grandchild weight should be updated",
            0.75f, updatedGrandchild.weight.value, 0.001f);
        assertTrue("Grandchild priority should be set",
            updatedGrandchild.priority.isPresent());
        assertEquals("Grandchild priority should be correct",
            0.9f, updatedGrandchild.priority.get().value, 0.001f);
        assertTrue("Grandchild alias should be set",
            updatedGrandchild.alias.isPresent());
        assertEquals("Grandchild alias should be correct",
            "http://example.org", updatedGrandchild.alias.get());
    }

    @Test
    public void testUpdateWithHeight1DoesNotUpdateGrandchildren() throws Exception {
        // Create root -> child -> grandchild hierarchy
        AtomId rootId = net.fortytwo.smsn.SemanticSynchrony.createRandomId();
        AtomId childId = net.fortytwo.smsn.SemanticSynchrony.createRandomId();
        AtomId grandchildId = net.fortytwo.smsn.SemanticSynchrony.createRandomId();

        Atom root = repository.createAtom(rootId,
            new net.fortytwo.smsn.brain.SourceName("public"),
            "root");
        Atom child = repository.createAtom(childId,
            new net.fortytwo.smsn.brain.SourceName("public"),
            "child");
        Atom grandchild = repository.createAtom(grandchildId,
            new net.fortytwo.smsn.brain.SourceName("public"),
            "grandchild original");

        // Link: root -> child -> grandchild
        repository.addChildAt(rootId, childId, 0);
        repository.addChildAt(childId, grandchildId, 0);

        // Create tree update with modified grandchild title
        TreeNode grandchildNode = new TreeNode(
            grandchildId,
            grandchild.created,
            grandchild.weight,
            grandchild.priority,
            grandchild.source,
            "grandchild modified",  // Changed title
            grandchild.alias,
            grandchild.text,
            grandchild.shortcut,
            new ArrayList<>(),
            0,
            0
        );

        TreeNode childNode = new TreeNode(
            childId,
            child.created,
            child.weight,
            child.priority,
            child.source,
            "child modified",  // Changed title
            child.alias,
            child.text,
            child.shortcut,
            List.of(grandchildNode),
            1,
            0
        );

        TreeNode rootNode = new TreeNode(
            rootId,
            root.created,
            root.weight,
            root.priority,
            root.source,
            "root",
            root.alias,
            root.text,
            root.shortcut,
            List.of(childNode),
            1,
            0
        );

        // Update with height=1 (only reaches children, not grandchildren)
        updater.update(rootNode, 1, Filter.noFilter());

        // Verify child title was updated
        Atom updatedChild = repository.load(childId);
        assertEquals("Child title should be updated", "child modified", updatedChild.title);

        // Verify grandchild title was NOT updated (height didn't reach it)
        Atom updatedGrandchild = repository.load(grandchildId);
        assertEquals("Grandchild title should NOT be updated",
            "grandchild original", updatedGrandchild.title);
    }
}
