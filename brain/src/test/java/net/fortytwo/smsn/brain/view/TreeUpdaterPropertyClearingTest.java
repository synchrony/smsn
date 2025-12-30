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

import static org.junit.Assert.*;

/**
 * Tests for clearing properties via empty values (e.g., "@alias" alone).
 */
public class TreeUpdaterPropertyClearingTest extends BrainTestBase {
    private AtomRepository repository;
    private TreeUpdater updater;

    @Before
    public void setUp() throws Exception {
        super.setUp();
        repository = brain.getAtomRepository();
        updater = new TreeUpdater(repository, null);
    }

    @Test
    public void testClearAlias() throws Exception {
        // Create atom with alias
        AtomId atomId = net.fortytwo.smsn.SemanticSynchrony.createRandomId();
        Atom atom = repository.createAtom(atomId,
            new net.fortytwo.smsn.brain.SourceName("private"),
            "test atom");

        atom = atom.withAlias(hydra.util.Opt.of("https://example.org"));
        repository.save(atom);

        // Verify alias is set
        Atom reloaded = repository.load(atomId);
        assertTrue("Alias should be present", reloaded.alias.isPresent());
        assertEquals("Alias should be correct", "https://example.org", reloaded.alias.get());

        // Update with empty alias (to clear it)
        TreeNode treeUpdate = new TreeNode(
            atomId,
            reloaded.created,
            reloaded.weight,
            reloaded.priority,
            reloaded.source,
            reloaded.title,
            hydra.util.Opt.of(""),  // Empty string means "clear"
            reloaded.text,
            reloaded.shortcut,
            new ArrayList<>(),
            0,
            0
        );

        updater.update(treeUpdate, 1, Filter.noFilter());

        // Verify alias is cleared
        Atom updated = repository.load(atomId);
        assertFalse("Alias should be cleared", updated.alias.isPresent());
    }

    @Test
    public void testClearText() throws Exception {
        // Create atom with text
        AtomId atomId = net.fortytwo.smsn.SemanticSynchrony.createRandomId();
        Atom atom = repository.createAtom(atomId,
            new net.fortytwo.smsn.brain.SourceName("private"),
            "test atom");

        atom = atom.withText(hydra.util.Opt.of("Some descriptive text"));
        repository.save(atom);

        // Verify text is set
        Atom reloaded = repository.load(atomId);
        assertTrue("Text should be present", reloaded.text.isPresent());
        assertEquals("Text should be correct", "Some descriptive text", reloaded.text.get());

        // Update with empty text (to clear it)
        TreeNode treeUpdate = new TreeNode(
            atomId,
            reloaded.created,
            reloaded.weight,
            reloaded.priority,
            reloaded.source,
            reloaded.title,
            reloaded.alias,
            hydra.util.Opt.of(""),  // Empty string means "clear"
            reloaded.shortcut,
            new ArrayList<>(),
            0,
            0
        );

        updater.update(treeUpdate, 1, Filter.noFilter());

        // Verify text is cleared
        Atom updated = repository.load(atomId);
        assertFalse("Text should be cleared", updated.text.isPresent());
    }

    @Test
    public void testClearPriority() throws Exception {
        // Create atom with priority
        AtomId atomId = net.fortytwo.smsn.SemanticSynchrony.createRandomId();
        Atom atom = repository.createAtom(atomId,
            new net.fortytwo.smsn.brain.SourceName("private"),
            "test atom");

        atom = atom.withPriority(hydra.util.Opt.of(new net.fortytwo.smsn.brain.Normed(0.75f)));
        repository.save(atom);

        // Verify priority is set
        Atom reloaded = repository.load(atomId);
        assertTrue("Priority should be present", reloaded.priority.isPresent());
        assertEquals("Priority should be correct", 0.75f, reloaded.priority.get().value, 0.001f);

        // Update with sentinel priority value (to clear it)
        TreeNode treeUpdate = new TreeNode(
            atomId,
            reloaded.created,
            reloaded.weight,
            hydra.util.Opt.of(new net.fortytwo.smsn.brain.Normed(-1.0f)),  // Negative means "clear"
            reloaded.source,
            reloaded.title,
            reloaded.alias,
            reloaded.text,
            reloaded.shortcut,
            new ArrayList<>(),
            0,
            0
        );

        updater.update(treeUpdate, 1, Filter.noFilter());

        // Verify priority is cleared
        Atom updated = repository.load(atomId);
        assertFalse("Priority should be cleared", updated.priority.isPresent());
    }

    @Test
    public void testClearShortcut() throws Exception {
        // Create atom with shortcut
        AtomId atomId = net.fortytwo.smsn.SemanticSynchrony.createRandomId();
        Atom atom = repository.createAtom(atomId,
            new net.fortytwo.smsn.brain.SourceName("private"),
            "test atom");

        atom = atom.withShortcut(hydra.util.Opt.of("ABC"));
        repository.save(atom);

        // Verify shortcut is set
        Atom reloaded = repository.load(atomId);
        assertTrue("Shortcut should be present", reloaded.shortcut.isPresent());
        assertEquals("Shortcut should be correct", "ABC", reloaded.shortcut.get());

        // Update with empty shortcut (to clear it)
        TreeNode treeUpdate = new TreeNode(
            atomId,
            reloaded.created,
            reloaded.weight,
            reloaded.priority,
            reloaded.source,
            reloaded.title,
            reloaded.alias,
            reloaded.text,
            hydra.util.Opt.of(""),  // Empty string means "clear"
            new ArrayList<>(),
            0,
            0
        );

        updater.update(treeUpdate, 1, Filter.noFilter());

        // Verify shortcut is cleared
        Atom updated = repository.load(atomId);
        assertFalse("Shortcut should be cleared", updated.shortcut.isPresent());
    }
}
