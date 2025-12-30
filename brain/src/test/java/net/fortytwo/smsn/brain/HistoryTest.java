package net.fortytwo.smsn.brain;

import net.fortytwo.smsn.brain.model.Filter;
import net.fortytwo.smsn.brain.repository.AtomRepository;
import org.junit.Before;
import org.junit.Test;

import java.util.List;

import static org.junit.Assert.*;

/**
 * Tests for History - the visit history tracking system.
 */
public class HistoryTest extends BrainTestBase {
    private History history;
    private AtomRepository repository;

    @Before
    public void setUp() throws Exception {
        super.setUp();
        history = new History();
        repository = brain.getAtomRepository();
    }

    @Test
    public void visitAddsToHistory() {
        AtomId id = new AtomId("test-id");
        history.visit(id);

        List<AtomId> ids = history.getHistoryIds(10);
        assertEquals(1, ids.size());
        assertEquals("test-id", ids.get(0).value);
    }

    @Test
    public void multipleVisitsRecorded() {
        history.visit(new AtomId("id1"));
        history.visit(new AtomId("id2"));
        history.visit(new AtomId("id3"));

        List<AtomId> ids = history.getHistoryIds(10);
        assertEquals(3, ids.size());
    }

    @Test
    public void historyReturnsMostRecentFirst() {
        history.visit(new AtomId("first"));
        history.visit(new AtomId("second"));
        history.visit(new AtomId("third"));

        List<AtomId> ids = history.getHistoryIds(10);
        assertEquals("third", ids.get(0).value);
        assertEquals("second", ids.get(1).value);
        assertEquals("first", ids.get(2).value);
    }

    @Test
    public void consecutiveSameIdNotDuplicated() {
        AtomId id = new AtomId("same-id");
        history.visit(id);
        history.visit(id);
        history.visit(id);

        List<AtomId> ids = history.getHistoryIds(10);
        assertEquals(1, ids.size());
    }

    @Test
    public void nonConsecutiveSameIdIsDuplicated() {
        history.visit(new AtomId("a"));
        history.visit(new AtomId("b"));
        history.visit(new AtomId("a"));  // Revisit a

        List<AtomId> ids = history.getHistoryIds(10);
        assertEquals(3, ids.size());
        assertEquals("a", ids.get(0).value);  // Most recent
        assertEquals("b", ids.get(1).value);
        assertEquals("a", ids.get(2).value);  // Original visit
    }

    @Test
    public void getHistoryIdsRespectsMaxLen() {
        for (int i = 0; i < 10; i++) {
            history.visit(new AtomId("id" + i));
        }

        List<AtomId> ids = history.getHistoryIds(5);
        assertEquals(5, ids.size());
        // Should be most recent 5
        assertEquals("id9", ids.get(0).value);
        assertEquals("id8", ids.get(1).value);
    }

    @Test
    public void emptyHistoryReturnsEmptyList() {
        List<AtomId> ids = history.getHistoryIds(10);
        assertEquals(0, ids.size());
    }

    @Test
    public void getHistoryReturnsAtoms() {
        // Create actual atoms in the repository
        Atom atom1 = repository.createAtom(new AtomId("atom1"), new SourceName("private"), "First");
        Atom atom2 = repository.createAtom(new AtomId("atom2"), new SourceName("private"), "Second");

        history.visit(atom1.id);
        history.visit(atom2.id);

        List<Atom> atoms = history.getHistory(10, repository, null);
        assertEquals(2, atoms.size());
        assertEquals("Second", atoms.get(0).title);  // Most recent first
        assertEquals("First", atoms.get(1).title);
    }

    @Test
    public void getHistorySkipsDeletedAtoms() {
        // Create atoms
        Atom atom1 = repository.createAtom(new AtomId("exists"), new SourceName("private"), "Exists");
        repository.createAtom(new AtomId("deleted"), new SourceName("private"), "Deleted");

        history.visit(atom1.id);
        history.visit(new AtomId("deleted"));

        // Delete the second atom
        repository.delete(new AtomId("deleted"));

        // History should only contain the existing atom
        List<Atom> atoms = history.getHistory(10, repository, null);
        assertEquals(1, atoms.size());
        assertEquals("Exists", atoms.get(0).title);
    }

    @Test
    public void getHistoryRespectsFilter() {
        // Create atoms with different weights
        Atom highWeight = repository.createAtom(new AtomId("high"), new SourceName("private"), "High");
        highWeight = highWeight.withWeight(new Normed(0.9f));
        repository.save(highWeight);

        Atom lowWeight = repository.createAtom(new AtomId("low"), new SourceName("private"), "Low");
        lowWeight = lowWeight.withWeight(new Normed(0.1f));
        repository.save(lowWeight);

        history.visit(highWeight.id);
        history.visit(lowWeight.id);

        // Filter for weight >= 0.5
        Filter filter = new Filter(0.5f, 0.5f, "private", "private");
        List<Atom> atoms = history.getHistory(10, repository, filter);

        assertEquals(1, atoms.size());
        assertEquals("High", atoms.get(0).title);
    }

    @Test
    public void getHistoryRespectsMaxLen() {
        // Create atoms
        for (int i = 0; i < 10; i++) {
            Atom atom = repository.createAtom(new AtomId("atom" + i), new SourceName("private"), "Atom " + i);
            history.visit(atom.id);
        }

        List<Atom> atoms = history.getHistory(5, repository, null);
        assertEquals(5, atoms.size());
    }

    @Test
    public void getHistoryWithNoFilterPassesAll() {
        Atom atom = repository.createAtom(new AtomId("test"), new SourceName("private"), "Test");
        history.visit(atom.id);

        List<Atom> atoms = history.getHistory(10, repository, null);
        assertEquals(1, atoms.size());
    }
}
