package net.fortytwo.smsn.brain.repository;

import net.fortytwo.smsn.SemanticSynchrony;
import net.fortytwo.smsn.brain.Atom;
import net.fortytwo.smsn.brain.AtomId;
import net.fortytwo.smsn.brain.BrainTestBase;
import net.fortytwo.smsn.brain.Normed;
import net.fortytwo.smsn.brain.SourceName;
import net.fortytwo.smsn.brain.model.Filter;
import org.junit.Before;
import org.junit.Test;

import java.util.Arrays;
import java.util.List;
import java.util.Optional;

import static org.junit.Assert.*;

/**
 * Tests for AtomRepository - the core data access layer for atoms.
 */
public class AtomRepositoryTest extends BrainTestBase {
    private AtomRepository repository;

    @Before
    public void setUp() throws Exception {
        super.setUp();
        repository = brain.getAtomRepository();
    }

    // ==================== CRUD Operations ====================

    @Test
    public void createAtomWithIdAndTitle() {
        AtomId id = SemanticSynchrony.createRandomId();
        Atom atom = repository.createAtom(id, new SourceName("private"), "Test Atom");

        assertNotNull(atom);
        assertEquals(id, atom.id);
        assertEquals("Test Atom", atom.title);
        assertEquals("private", atom.source.value);
        assertEquals(0.5f, atom.weight.value, 0.001f);  // default weight
    }

    @Test
    public void loadReturnsCreatedAtom() {
        AtomId id = SemanticSynchrony.createRandomId();
        repository.createAtom(id, new SourceName("private"), "Test");

        Atom loaded = repository.load(id);

        assertEquals(id, loaded.id);
        assertEquals("Test", loaded.title);
    }

    @Test(expected = IllegalArgumentException.class)
    public void loadThrowsForNonexistentAtom() {
        repository.load(new AtomId("nonexistent"));
    }

    @Test
    public void findByIdReturnsEmptyForNonexistent() {
        Optional<Atom> result = repository.findById(new AtomId("nonexistent"));
        assertFalse(result.isPresent());
    }

    @Test
    public void findByIdReturnsAtomWhenExists() {
        AtomId id = SemanticSynchrony.createRandomId();
        repository.createAtom(id, new SourceName("private"), "Test");

        Optional<Atom> result = repository.findById(id);

        assertTrue(result.isPresent());
        assertEquals("Test", result.get().title);
    }

    @Test
    public void saveUpdatesExistingAtom() {
        AtomId id = SemanticSynchrony.createRandomId();
        Atom original = repository.createAtom(id, new SourceName("private"), "Original");

        Atom modified = original.withTitle("Modified").withWeight(new Normed(0.8f));
        repository.save(modified);

        Atom reloaded = repository.load(id);
        assertEquals("Modified", reloaded.title);
        assertEquals(0.8f, reloaded.weight.value, 0.001f);
    }

    @Test
    public void deleteRemovesAtom() {
        AtomId id = SemanticSynchrony.createRandomId();
        repository.createAtom(id, new SourceName("private"), "To Delete");

        repository.delete(id);

        assertFalse(repository.findById(id).isPresent());
    }

    @Test
    public void deleteNonexistentAtomDoesNotThrow() {
        // Should not throw
        repository.delete(new AtomId("nonexistent"));
    }

    // ==================== Child List Operations ====================

    @Test
    public void addChildAtCreatesChildRelation() {
        Atom parent = createAtom("parent");
        Atom child = createAtom("child");

        repository.addChildAt(parent.id, child.id, 0);

        List<AtomId> children = repository.getChildrenIds(parent.id);
        assertEquals(1, children.size());
        assertEquals(child.id, children.get(0));
    }

    @Test
    public void addChildAtInsertsAtCorrectPosition() {
        Atom parent = createAtom("parent");
        Atom first = createAtom("first");
        Atom second = createAtom("second");
        Atom third = createAtom("third");

        repository.addChildAt(parent.id, first.id, 0);
        repository.addChildAt(parent.id, third.id, 1);
        repository.addChildAt(parent.id, second.id, 1);  // Insert in middle

        List<AtomId> children = repository.getChildrenIds(parent.id);
        assertEquals(3, children.size());
        assertEquals(first.id, children.get(0));
        assertEquals(second.id, children.get(1));
        assertEquals(third.id, children.get(2));
    }

    @Test
    public void deleteChildAtRemovesFromList() {
        Atom parent = createAtom("parent");
        Atom first = createAtom("first");
        Atom second = createAtom("second");
        Atom third = createAtom("third");

        repository.addChildAt(parent.id, first.id, 0);
        repository.addChildAt(parent.id, second.id, 1);
        repository.addChildAt(parent.id, third.id, 2);

        repository.deleteChildAt(parent.id, 1);  // Remove middle

        List<AtomId> children = repository.getChildrenIds(parent.id);
        assertEquals(2, children.size());
        assertEquals(first.id, children.get(0));
        assertEquals(third.id, children.get(1));
    }

    @Test
    public void deleteChildAtHeadWorks() {
        Atom parent = createAtom("parent");
        Atom first = createAtom("first");
        Atom second = createAtom("second");

        repository.addChildAt(parent.id, first.id, 0);
        repository.addChildAt(parent.id, second.id, 1);

        repository.deleteChildAt(parent.id, 0);  // Remove head

        List<AtomId> children = repository.getChildrenIds(parent.id);
        assertEquals(1, children.size());
        assertEquals(second.id, children.get(0));
    }

    @Test
    public void setChildrenReplacesAllChildren() {
        Atom parent = createAtom("parent");
        Atom oldChild = createAtom("old");
        Atom newChild1 = createAtom("new1");
        Atom newChild2 = createAtom("new2");

        repository.addChildAt(parent.id, oldChild.id, 0);

        repository.setChildren(parent.id, Arrays.asList(newChild1.id, newChild2.id));

        List<AtomId> children = repository.getChildrenIds(parent.id);
        assertEquals(2, children.size());
        assertEquals(newChild1.id, children.get(0));
        assertEquals(newChild2.id, children.get(1));
    }

    @Test
    public void getChildrenReturnsFullAtoms() {
        Atom parent = createAtom("parent");
        Atom child = createAtom("child");
        repository.addChildAt(parent.id, child.id, 0);

        List<Atom> children = repository.getChildren(parent.id);

        assertEquals(1, children.size());
        assertEquals("child", children.get(0).title);
    }

    // ==================== Parent Navigation ====================

    @Test
    public void countParentsReturnsCorrectCount() {
        Atom parent1 = createAtom("parent1");
        Atom parent2 = createAtom("parent2");
        Atom child = createAtom("child");

        repository.addChildAt(parent1.id, child.id, 0);
        repository.addChildAt(parent2.id, child.id, 0);

        assertEquals(2, repository.countParents(child.id));
    }

    @Test
    public void countParentsReturnsZeroForOrphans() {
        Atom orphan = createAtom("orphan");
        assertEquals(0, repository.countParents(orphan.id));
    }

    @Test
    public void getParentIdsReturnsAllParents() {
        Atom parent1 = createAtom("parent1");
        Atom parent2 = createAtom("parent2");
        Atom child = createAtom("child");

        repository.addChildAt(parent1.id, child.id, 0);
        repository.addChildAt(parent2.id, child.id, 0);

        List<AtomId> parents = repository.getParentIds(child.id);

        assertEquals(2, parents.size());
        assertTrue(parents.contains(parent1.id));
        assertTrue(parents.contains(parent2.id));
    }

    // ==================== Property Updates ====================

    @Test
    public void updatePropertyChangesTitle() {
        Atom atom = createAtom("original");

        repository.updateProperty(atom.id, SemanticSynchrony.PropertyKeys.TITLE, "updated");

        Atom reloaded = repository.load(atom.id);
        assertEquals("updated", reloaded.title);
    }

    @Test
    public void updatePropertyChangesWeight() {
        Atom atom = createAtom("test");

        repository.updateProperty(atom.id, SemanticSynchrony.PropertyKeys.WEIGHT, 0.75f);

        Atom reloaded = repository.load(atom.id);
        assertEquals(0.75f, reloaded.weight.value, 0.001f);
    }

    @Test
    public void updatePropertyHandlesDoubleToFloat() {
        Atom atom = createAtom("test");

        // JSON parsing typically produces Double, not Float
        repository.updateProperty(atom.id, SemanticSynchrony.PropertyKeys.WEIGHT, 0.75);

        Atom reloaded = repository.load(atom.id);
        assertEquals(0.75f, reloaded.weight.value, 0.001f);
    }

    @Test
    public void updatePropertySetsText() {
        Atom atom = createAtom("test");

        repository.updateProperty(atom.id, SemanticSynchrony.PropertyKeys.TEXT, "Extended text content");

        Atom reloaded = repository.load(atom.id);
        assertTrue(reloaded.text.isPresent());
        assertEquals("Extended text content", reloaded.text.get());
    }

    @Test
    public void updatePropertyClearsOptionalProperty() {
        Atom atom = createAtom("test");
        repository.updateProperty(atom.id, SemanticSynchrony.PropertyKeys.TEXT, "initial");

        repository.updateProperty(atom.id, SemanticSynchrony.PropertyKeys.TEXT, null);

        Atom reloaded = repository.load(atom.id);
        assertFalse(reloaded.text.isPresent());
    }

    // ==================== Bulk Operations ====================

    @Test
    public void getAllAtomIdsReturnsAllAtoms() {
        createAtom("a");
        createAtom("b");
        createAtom("c");

        List<AtomId> ids = repository.getAllAtomIds();

        assertEquals(3, ids.size());
    }

    @Test
    public void getAllAtomIdsExcludesListNodes() {
        Atom parent = createAtom("parent");
        Atom child = createAtom("child");
        repository.addChildAt(parent.id, child.id, 0);

        // Should only return 2 (parent and child), not the LIST node
        List<AtomId> ids = repository.getAllAtomIds();
        assertEquals(2, ids.size());
    }

    // ==================== Filter Tests ====================

    @Test
    public void testFilterPassesMatchingAtom() {
        Atom atom = createAtom("test");
        atom = atom.withWeight(new Normed(0.8f));
        repository.save(atom);

        Filter filter = new Filter(0.5f, 0.5f, "private", "private");
        assertTrue(repository.testFilter(repository.load(atom.id), filter));
    }

    @Test
    public void testFilterRejectsLowWeight() {
        Atom atom = createAtom("test");
        atom = atom.withWeight(new Normed(0.2f));
        repository.save(atom);

        Filter filter = new Filter(0.5f, 0.5f, "private", "private");
        assertFalse(repository.testFilter(repository.load(atom.id), filter));
    }

    @Test
    public void testFilterWithNullPassesAll() {
        Atom atom = createAtom("test");
        assertTrue(repository.testFilter(atom, null));
    }

    @Test
    public void testFilterWithTrivialFilterPassesAll() {
        Atom atom = createAtom("test");
        assertTrue(repository.testFilter(atom, Filter.noFilter()));
    }

    // ==================== Create With Filter ====================

    @Test
    public void createAtomWithFilterUsesDefaultSource() {
        Filter filter = new Filter(0.5f, 0.5f, "private", "personal");

        Atom atom = repository.createAtom(filter);

        assertEquals("personal", atom.source.value);  // Uses defaultSource from filter
    }

    @Test
    public void createAtomWithFilterUsesDefaultWeight() {
        Filter filter = new Filter(0.5f, 0.75f, "private", "private");

        Atom atom = repository.createAtom(filter);

        assertEquals(0.75f, atom.weight.value, 0.001f);
    }

    // ==================== Update Tracking ====================

    @Test
    public void notifyOfUpdateChangesTimestamp() throws InterruptedException {
        long before = repository.getLastUpdate();
        Thread.sleep(10);  // Ensure time passes

        repository.notifyOfUpdate();

        long after = repository.getLastUpdate();
        assertTrue(after > before);
    }

    // ==================== Helper Methods ====================

    private Atom createAtom(String title) {
        AtomId id = SemanticSynchrony.createRandomId();
        return repository.createAtom(id, new SourceName("private"), title);
    }
}
