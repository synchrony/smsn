package net.fortytwo.smsn.server.actions;

import net.fortytwo.smsn.SemanticSynchrony;
import net.fortytwo.smsn.brain.Atom;
import net.fortytwo.smsn.brain.AtomId;
import net.fortytwo.smsn.brain.SourceName;
import net.fortytwo.smsn.brain.model.Filter;
import net.fortytwo.smsn.brain.repository.AtomRepository;
import net.fortytwo.smsn.server.ActionContext;
import org.junit.Before;
import org.junit.Test;

import static org.junit.Assert.*;

public class RemoveIsolatedNotesTest extends ActionTestBase {
    private AtomRepository repository;

    @Before
    public void setUp() throws Exception {
        super.setUp();
        repository = brain.getAtomRepository();
    }

    @Test
    public void removesIsolatedNotes() throws Exception {
        // Create isolated note (no parents, no children)
        Atom isolated = createAtom("isolated");

        // Create connected notes
        Atom parent = createAtom("parent");
        Atom child = createAtom("child");
        repository.addChildAt(parent.id, child.id, 0);

        assertEquals(3, repository.getAllAtomIds().size());

        perform(createAction());

        // Only isolated note should be removed
        assertEquals(2, repository.getAllAtomIds().size());
        assertFalse(repository.findById(isolated.id).isPresent());
        assertTrue(repository.findById(parent.id).isPresent());
        assertTrue(repository.findById(child.id).isPresent());
    }

    @Test
    public void leavesConnectedNotesAlone() throws Exception {
        // Create a chain: grandparent -> parent -> child
        Atom grandparent = createAtom("grandparent");
        Atom parent = createAtom("parent");
        Atom child = createAtom("child");
        repository.addChildAt(grandparent.id, parent.id, 0);
        repository.addChildAt(parent.id, child.id, 0);

        perform(createAction());

        // All notes should remain (none are isolated)
        assertEquals(3, repository.getAllAtomIds().size());
    }

    @Test
    public void noteWithOnlyChildrenIsNotIsolated() throws Exception {
        Atom parent = createAtom("parent");
        Atom child = createAtom("child");
        repository.addChildAt(parent.id, child.id, 0);

        perform(createAction());

        // Parent has children, so it's not isolated
        assertTrue(repository.findById(parent.id).isPresent());
    }

    @Test
    public void noteWithOnlyParentsIsNotIsolated() throws Exception {
        Atom parent = createAtom("parent");
        Atom child = createAtom("child");
        repository.addChildAt(parent.id, child.id, 0);

        perform(createAction());

        // Child has parents, so it's not isolated
        assertTrue(repository.findById(child.id).isPresent());
    }

    @Test
    public void removesMultipleIsolatedNotes() throws Exception {
        createAtom("isolated1");
        createAtom("isolated2");
        createAtom("isolated3");

        assertEquals(3, repository.getAllAtomIds().size());

        perform(createAction());

        assertEquals(0, repository.getAllAtomIds().size());
    }

    @Test
    public void emptyGraphRemainsEmpty() throws Exception {
        assertEquals(0, repository.getAllAtomIds().size());

        perform(createAction());

        assertEquals(0, repository.getAllAtomIds().size());
    }

    private Atom createAtom(String title) {
        AtomId id = SemanticSynchrony.createRandomId();
        return repository.createAtom(id, new SourceName("private"), title);
    }

    private RemoveIsolatedNotes createAction() {
        RemoveIsolatedNotes action = new RemoveIsolatedNotes();
        action.setFilter(Filter.noFilter());
        return action;
    }
}
