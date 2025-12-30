package net.fortytwo.smsn.brain.repository;

import net.fortytwo.smsn.brain.Atom;
import net.fortytwo.smsn.brain.AtomId;
import net.fortytwo.smsn.brain.Brain;
import net.fortytwo.smsn.brain.BrainTestBase;
import org.junit.Before;
import org.junit.Test;

import java.util.List;

import static org.junit.Assert.*;

/**
 * Test for AtomRepository child list manipulation operations.
 * These tests ensure that addChildAt() and deleteChildAt() work correctly
 * and maintain the linked list structure efficiently.
 */
public class AtomRepositoryChildListTest extends BrainTestBase {
    private AtomRepository repository;

    @Before
    public void setUp() throws Exception {
        super.setUp();
        repository = brain.getAtomRepository();
    }

    @Test
    public void testAddChildToEmptyList() throws Exception {
        // Create parent and child atoms
        Atom parent = createAtom("parent");
        Atom child = createAtom("child1");

        // Parent should have no children initially
        assertEquals(0, parent.children.size());

        // Add child at position 0
        repository.addChildAt(parent.id, child.id, 0);

        // Reload parent and verify
        Atom updatedParent = repository.load(parent.id);
        assertEquals(1, updatedParent.children.size());
        assertEquals(child.id, updatedParent.children.get(0));
    }

    @Test
    public void testAddChildAtHead() throws Exception {
        // Create parent with two children
        Atom parent = createAtom("parent");
        Atom child1 = createAtom("child1");
        Atom child2 = createAtom("child2");

        repository.addChildAt(parent.id, child1.id, 0);
        repository.addChildAt(parent.id, child2.id, 0);

        // Reload and verify order: child2 should be first
        Atom updatedParent = repository.load(parent.id);
        assertEquals(2, updatedParent.children.size());
        assertEquals(child2.id, updatedParent.children.get(0));
        assertEquals(child1.id, updatedParent.children.get(1));
    }

    @Test
    public void testAddChildAtEnd() throws Exception {
        // Create parent with two children
        Atom parent = createAtom("parent");
        Atom child1 = createAtom("child1");
        Atom child2 = createAtom("child2");
        Atom child3 = createAtom("child3");

        repository.addChildAt(parent.id, child1.id, 0);
        repository.addChildAt(parent.id, child2.id, 1);
        repository.addChildAt(parent.id, child3.id, 2);

        // Reload and verify order
        Atom updatedParent = repository.load(parent.id);
        assertEquals(3, updatedParent.children.size());
        assertEquals(child1.id, updatedParent.children.get(0));
        assertEquals(child2.id, updatedParent.children.get(1));
        assertEquals(child3.id, updatedParent.children.get(2));
    }

    @Test
    public void testAddChildInMiddle() throws Exception {
        // Create parent with two children
        Atom parent = createAtom("parent");
        Atom child1 = createAtom("child1");
        Atom child2 = createAtom("child2");
        Atom child3 = createAtom("child3");

        repository.addChildAt(parent.id, child1.id, 0);
        repository.addChildAt(parent.id, child3.id, 1);
        // Insert child2 between child1 and child3
        repository.addChildAt(parent.id, child2.id, 1);

        // Reload and verify order
        Atom updatedParent = repository.load(parent.id);
        assertEquals(3, updatedParent.children.size());
        assertEquals(child1.id, updatedParent.children.get(0));
        assertEquals(child2.id, updatedParent.children.get(1));
        assertEquals(child3.id, updatedParent.children.get(2));
    }

    @Test
    public void testDeleteChildFromHead() throws Exception {
        // Create parent with three children
        Atom parent = createAtom("parent");
        Atom child1 = createAtom("child1");
        Atom child2 = createAtom("child2");
        Atom child3 = createAtom("child3");

        repository.addChildAt(parent.id, child1.id, 0);
        repository.addChildAt(parent.id, child2.id, 1);
        repository.addChildAt(parent.id, child3.id, 2);

        // Delete first child
        repository.deleteChildAt(parent.id, 0);

        // Reload and verify
        Atom updatedParent = repository.load(parent.id);
        assertEquals(2, updatedParent.children.size());
        assertEquals(child2.id, updatedParent.children.get(0));
        assertEquals(child3.id, updatedParent.children.get(1));
    }

    @Test
    public void testDeleteChildFromEnd() throws Exception {
        // Create parent with three children
        Atom parent = createAtom("parent");
        Atom child1 = createAtom("child1");
        Atom child2 = createAtom("child2");
        Atom child3 = createAtom("child3");

        repository.addChildAt(parent.id, child1.id, 0);
        repository.addChildAt(parent.id, child2.id, 1);
        repository.addChildAt(parent.id, child3.id, 2);

        // Delete last child
        repository.deleteChildAt(parent.id, 2);

        // Reload and verify
        Atom updatedParent = repository.load(parent.id);
        assertEquals(2, updatedParent.children.size());
        assertEquals(child1.id, updatedParent.children.get(0));
        assertEquals(child2.id, updatedParent.children.get(1));
    }

    @Test
    public void testDeleteChildFromMiddle() throws Exception {
        // Create parent with three children
        Atom parent = createAtom("parent");
        Atom child1 = createAtom("child1");
        Atom child2 = createAtom("child2");
        Atom child3 = createAtom("child3");

        repository.addChildAt(parent.id, child1.id, 0);
        repository.addChildAt(parent.id, child2.id, 1);
        repository.addChildAt(parent.id, child3.id, 2);

        // Delete middle child
        repository.deleteChildAt(parent.id, 1);

        // Reload and verify
        Atom updatedParent = repository.load(parent.id);
        assertEquals(2, updatedParent.children.size());
        assertEquals(child1.id, updatedParent.children.get(0));
        assertEquals(child3.id, updatedParent.children.get(1));
    }

    @Test
    public void testDeleteAllChildren() throws Exception {
        // Create parent with two children
        Atom parent = createAtom("parent");
        Atom child1 = createAtom("child1");
        Atom child2 = createAtom("child2");

        repository.addChildAt(parent.id, child1.id, 0);
        repository.addChildAt(parent.id, child2.id, 1);

        // Delete both children
        repository.deleteChildAt(parent.id, 1);
        repository.deleteChildAt(parent.id, 0);

        // Reload and verify empty list
        Atom updatedParent = repository.load(parent.id);
        assertEquals(0, updatedParent.children.size());
    }

    @Test(expected = IllegalArgumentException.class)
    public void testAddChildToNonexistentParent() throws Exception {
        Atom child = createAtom("child");
        repository.addChildAt(new AtomId("nonexistent"), child.id, 0);
    }

    @Test(expected = IllegalArgumentException.class)
    public void testAddNonexistentChild() throws Exception {
        Atom parent = createAtom("parent");
        repository.addChildAt(parent.id, new AtomId("nonexistent"), 0);
    }

    @Test(expected = IllegalArgumentException.class)
    public void testDeleteFromEmptyList() throws Exception {
        Atom parent = createAtom("parent");
        repository.deleteChildAt(parent.id, 0);
    }

    @Test(expected = IllegalArgumentException.class)
    public void testDeleteAtInvalidPosition() throws Exception {
        Atom parent = createAtom("parent");
        Atom child = createAtom("child");
        repository.addChildAt(parent.id, child.id, 0);

        // Try to delete at position 5 when list only has 1 element
        repository.deleteChildAt(parent.id, 5);
    }

    @Test
    public void testMultipleOperations() throws Exception {
        // Create a complex scenario with multiple adds and deletes
        Atom parent = createAtom("parent");
        Atom child1 = createAtom("child1");
        Atom child2 = createAtom("child2");
        Atom child3 = createAtom("child3");
        Atom child4 = createAtom("child4");

        // Add three children
        repository.addChildAt(parent.id, child1.id, 0);
        repository.addChildAt(parent.id, child2.id, 1);
        repository.addChildAt(parent.id, child3.id, 2);

        // Delete middle one
        repository.deleteChildAt(parent.id, 1);

        // Add new child in middle
        repository.addChildAt(parent.id, child4.id, 1);

        // Final order should be: child1, child4, child3
        Atom updatedParent = repository.load(parent.id);
        assertEquals(3, updatedParent.children.size());
        assertEquals(child1.id, updatedParent.children.get(0));
        assertEquals(child4.id, updatedParent.children.get(1));
        assertEquals(child3.id, updatedParent.children.get(2));
    }

    @Test
    public void testChildAtomsRemainUnaffected() throws Exception {
        // Verify that manipulating the child list doesn't affect the child atoms themselves
        Atom parent = createAtom("parent");
        Atom child = createAtom("child");
        String originalTitle = child.title;

        repository.addChildAt(parent.id, child.id, 0);
        repository.deleteChildAt(parent.id, 0);

        // Child atom should still exist and be unchanged
        Atom reloadedChild = repository.load(child.id);
        assertNotNull(reloadedChild);
        assertEquals(originalTitle, reloadedChild.title);
    }

    private Atom createAtom(String title) {
        AtomId id = net.fortytwo.smsn.SemanticSynchrony.createRandomId();
        return repository.createAtom(id,
            new net.fortytwo.smsn.brain.SourceName("private"),
            title);
    }
}
