package net.fortytwo.smsn.brain.repository;

import hydra.util.Opt;
import net.fortytwo.smsn.SemanticSynchrony;
import net.fortytwo.smsn.brain.Atom;
import net.fortytwo.smsn.brain.AtomId;
import net.fortytwo.smsn.brain.Normed;
import net.fortytwo.smsn.brain.SourceName;
import net.fortytwo.smsn.brain.Timestamp;
import net.fortytwo.smsn.brain.model.Filter;
import net.fortytwo.smsn.config.Configuration;
import net.fortytwo.smsn.config.DataSource;
import org.junit.After;
import org.junit.Before;
import org.junit.Test;

import java.io.File;
import java.nio.file.Files;
import java.util.ArrayList;
import java.util.List;
import java.util.Optional;

import static org.junit.Assert.*;

/**
 * Tests for the file-based atom repository.
 */
public class FileBasedAtomRepositoryTest {

    private File tempDir;
    private File indexDir;
    private File sourceDir;
    private FileBasedAtomRepository repository;
    private Configuration originalConfig;

    @Before
    public void setUp() throws Exception {
        // Save original configuration
        originalConfig = SemanticSynchrony.getConfiguration();

        // Create temporary directories
        tempDir = Files.createTempDirectory("smsn-test").toFile();
        indexDir = new File(tempDir, "index");
        sourceDir = new File(tempDir, "atoms");
        sourceDir.mkdirs();

        // Configure SmSn with test source
        Configuration config = new Configuration();
        DataSource source = new DataSource();
        source.setName("test");
        source.setLocation(sourceDir.getAbsolutePath());
        config.setSources(List.of(source));
        SemanticSynchrony.setConfiguration(config);

        // Create repository
        repository = new FileBasedAtomRepository(indexDir);
        repository.initialize();
    }

    @After
    public void tearDown() throws Exception {
        if (repository != null) {
            repository.close();
        }

        // Restore original configuration
        if (originalConfig != null) {
            SemanticSynchrony.setConfiguration(originalConfig);
        }

        deleteRecursively(tempDir);
    }

    private void deleteRecursively(File file) {
        if (file.isDirectory()) {
            File[] children = file.listFiles();
            if (children != null) {
                for (File child : children) {
                    deleteRecursively(child);
                }
            }
        }
        file.delete();
    }

    @Test
    public void testCreateAndLoad() throws Exception {
        // Create an atom
        AtomId id = new AtomId("test123");
        Atom atom = repository.createAtom(id, new SourceName("test"), "Test Title");

        // Load it back
        Optional<Atom> loaded = repository.findById(id);
        assertTrue(loaded.isPresent());
        assertEquals("Test Title", loaded.get().title);
        assertEquals("test", loaded.get().source.value);
    }

    @Test
    public void testSaveAndLoad() throws Exception {
        // Create an atom with all properties
        AtomId id = new AtomId("full123");
        Atom atom = new Atom(
                id,
                new Timestamp(1000L),
                new Normed(0.75f),
                Opt.of(new Normed(0.5f)),
                new SourceName("test"),
                "Full Test",
                Opt.of("ft"),
                Opt.of("Some text content"),
                Opt.of("ftest"),
                new ArrayList<>()
        );

        repository.save(atom);
        repository.commit();

        // Load it back
        Atom loaded = repository.load(id);
        assertEquals("Full Test", loaded.title);
        assertEquals(0.75f, loaded.weight.value, 0.001);
        assertTrue(loaded.priority.isPresent());
        assertEquals(0.5f, loaded.priority.get().value, 0.001);
        assertTrue(loaded.alias.isPresent());
        assertEquals("ft", loaded.alias.get());
        assertTrue(loaded.text.isPresent());
        assertEquals("Some text content", loaded.text.get());
        assertTrue(loaded.shortcut.isPresent());
        assertEquals("ftest", loaded.shortcut.get());
    }

    @Test
    public void testUpdateProperty() throws Exception {
        AtomId id = new AtomId("update123");
        repository.createAtom(id, new SourceName("test"), "Original Title");

        repository.updateProperty(id, SemanticSynchrony.PropertyKeys.TITLE, "Updated Title");

        Atom loaded = repository.load(id);
        assertEquals("Updated Title", loaded.title);
    }

    @Test
    public void testChildren() throws Exception {
        // Create parent and children
        AtomId parentId = new AtomId("parent1");
        AtomId child1Id = new AtomId("child1");
        AtomId child2Id = new AtomId("child2");

        repository.createAtom(parentId, new SourceName("test"), "Parent");
        repository.createAtom(child1Id, new SourceName("test"), "Child 1");
        repository.createAtom(child2Id, new SourceName("test"), "Child 2");

        // Set children
        repository.setChildren(parentId, List.of(child1Id, child2Id));
        repository.commit();

        // Verify children
        List<AtomId> children = repository.getChildrenIds(parentId);
        assertEquals(2, children.size());
        assertEquals(child1Id, children.get(0));
        assertEquals(child2Id, children.get(1));

        // Verify parents
        List<AtomId> parents = repository.getParentIds(child1Id);
        assertEquals(1, parents.size());
        assertEquals(parentId, parents.get(0));
    }

    @Test
    public void testAddChildAt() throws Exception {
        AtomId parentId = new AtomId("parent2");
        AtomId child1Id = new AtomId("c1");
        AtomId child2Id = new AtomId("c2");
        AtomId child3Id = new AtomId("c3");

        repository.createAtom(parentId, new SourceName("test"), "Parent");
        repository.createAtom(child1Id, new SourceName("test"), "Child 1");
        repository.createAtom(child2Id, new SourceName("test"), "Child 2");
        repository.createAtom(child3Id, new SourceName("test"), "Child 3");

        repository.setChildren(parentId, List.of(child1Id, child3Id));
        repository.addChildAt(parentId, child2Id, 1);

        List<AtomId> children = repository.getChildrenIds(parentId);
        assertEquals(3, children.size());
        assertEquals(child1Id, children.get(0));
        assertEquals(child2Id, children.get(1));
        assertEquals(child3Id, children.get(2));
    }

    @Test
    public void testDeleteChildAt() throws Exception {
        AtomId parentId = new AtomId("parent3");
        AtomId child1Id = new AtomId("d1");
        AtomId child2Id = new AtomId("d2");

        repository.createAtom(parentId, new SourceName("test"), "Parent");
        repository.createAtom(child1Id, new SourceName("test"), "Child 1");
        repository.createAtom(child2Id, new SourceName("test"), "Child 2");

        repository.setChildren(parentId, List.of(child1Id, child2Id));
        repository.deleteChildAt(parentId, 0);

        List<AtomId> children = repository.getChildrenIds(parentId);
        assertEquals(1, children.size());
        assertEquals(child2Id, children.get(0));
    }

    @Test
    public void testSearch() throws Exception {
        repository.createAtom(new AtomId("s1"), new SourceName("test"), "Apple Pie Recipe");
        repository.createAtom(new AtomId("s2"), new SourceName("test"), "Banana Bread");
        repository.createAtom(new AtomId("s3"), new SourceName("test"), "Apple Crisp");
        repository.commit();

        // Search for "apple"
        List<Atom> results = repository.search("apple", null);
        assertEquals(2, results.size());
    }

    @Test
    public void testDelete() throws Exception {
        AtomId id = new AtomId("del123");
        repository.createAtom(id, new SourceName("test"), "To Delete");
        repository.commit();

        assertTrue(repository.findById(id).isPresent());

        repository.delete(id);
        repository.commit();

        assertFalse(repository.findById(id).isPresent());
    }

    @Test
    public void testFilePersistence() throws Exception {
        // Create an atom and commit
        AtomId id = new AtomId("persist123");
        repository.createAtom(id, new SourceName("test"), "Persistent Atom");
        repository.commit();

        // Close and reopen repository
        repository.close();
        repository = new FileBasedAtomRepository(indexDir);
        repository.initialize();

        // Atom should still be there
        Optional<Atom> loaded = repository.findById(id);
        assertTrue(loaded.isPresent());
        assertEquals("Persistent Atom", loaded.get().title);
    }
}
