package net.fortytwo.smsn.brain.io.wiki;

import net.fortytwo.smsn.SemanticSynchrony;
import net.fortytwo.smsn.brain.TreeNode;
import org.junit.Before;
import org.junit.Test;

import java.io.ByteArrayInputStream;
import java.io.InputStream;

import static org.junit.Assert.*;

/**
 * Tests for property parsing in TreeNodeWikiParser, especially top-level properties.
 */
public class TreeNodeWikiParserPropertiesTest {
    private TreeNodeWikiParser parser;

    @Before
    public void setUp() {
        parser = new TreeNodeWikiParser();
    }

    @Test
    public void testTopLevelWeightAndAlias() throws Exception {
        String wikiText = "@weight 1.0\n@alias https://example.org\n";
        InputStream in = new ByteArrayInputStream(wikiText.getBytes());

        TreeNode tree = parser.parse(in);

        // Top-level properties should be applied to the root
        assertNotNull("Tree should not be null", tree);
        assertEquals("Weight should be 1.0", 1.0f, tree.weight.value, 0.001f);
        assertTrue("Alias should be present", tree.alias.isPresent());
        assertEquals("Alias should be correct", "https://example.org", tree.alias.get());
        assertEquals("Root should have no children", 0, tree.children.size());
    }

    @Test
    public void testTopLevelWeightOnly() throws Exception {
        String wikiText = "@weight 0.75\n";
        InputStream in = new ByteArrayInputStream(wikiText.getBytes());

        TreeNode tree = parser.parse(in);

        assertNotNull("Tree should not be null", tree);
        assertEquals("Weight should be 0.75", 0.75f, tree.weight.value, 0.001f);
        assertFalse("Alias should not be present", tree.alias.isPresent());
    }

    @Test
    public void testChildWithProperties() throws Exception {
        String wikiText = "* child title\n@weight 0.25\n@alias http://child.example.org\n";
        InputStream in = new ByteArrayInputStream(wikiText.getBytes());

        TreeNode tree = parser.parse(in);

        assertNotNull("Tree should not be null", tree);
        assertEquals("Root should have one child", 1, tree.children.size());

        TreeNode child = tree.children.get(0);
        assertEquals("Child title should be correct", "child title", child.title);
        assertEquals("Child weight should be 0.25", 0.25f, child.weight.value, 0.001f);
        assertTrue("Child alias should be present", child.alias.isPresent());
        assertEquals("Child alias should be correct", "http://child.example.org", child.alias.get());
    }

    @Test
    public void testTopLevelAndChildProperties() throws Exception {
        String wikiText = "@weight 1.0\n* child\n@weight 0.25\n";
        InputStream in = new ByteArrayInputStream(wikiText.getBytes());

        TreeNode tree = parser.parse(in);

        assertNotNull("Tree should not be null", tree);
        assertEquals("Root weight should be 1.0", 1.0f, tree.weight.value, 0.001f);
        assertEquals("Root should have one child", 1, tree.children.size());

        TreeNode child = tree.children.get(0);
        assertEquals("Child title should be correct", "child", child.title);
        assertEquals("Child weight should be 0.25", 0.25f, child.weight.value, 0.001f);
    }

    @Test
    public void testAllPropertyTypes() throws Exception {
        String wikiText = "* test node\n" +
                "@weight 0.9\n" +
                "@priority 0.8\n" +
                "@alias http://example.org\n" +
                "@text some text content\n" +
                "@shortcut ABC\n";
        InputStream in = new ByteArrayInputStream(wikiText.getBytes());

        TreeNode tree = parser.parse(in);

        assertNotNull("Tree should not be null", tree);
        assertEquals("Root should have one child", 1, tree.children.size());

        TreeNode child = tree.children.get(0);
        assertEquals("Child title should be correct", "test node", child.title);
        assertEquals("Weight should be 0.9", 0.9f, child.weight.value, 0.001f);
        assertTrue("Priority should be present", child.priority.isPresent());
        assertEquals("Priority should be 0.8", 0.8f, child.priority.get().value, 0.001f);
        assertTrue("Alias should be present", child.alias.isPresent());
        assertEquals("Alias should be correct", "http://example.org", child.alias.get());
        assertTrue("Text should be present", child.text.isPresent());
        assertEquals("Text should be correct", "some text content", child.text.get());
        assertTrue("Shortcut should be present", child.shortcut.isPresent());
        assertEquals("Shortcut should be correct", "ABC", child.shortcut.get());
    }

    @Test
    public void testEmptyContentProducesEmptyRoot() throws Exception {
        String wikiText = "";
        InputStream in = new ByteArrayInputStream(wikiText.getBytes());

        TreeNode tree = parser.parse(in);

        assertNotNull("Tree should not be null", tree);
        assertEquals("Root title should be empty", "", tree.title);
        assertEquals("Root weight should be default", SemanticSynchrony.DEFAULT_WEIGHT, tree.weight.value, 0.001f);
        assertEquals("Root should have no children", 0, tree.children.size());
    }

    @Test
    public void testEmptyAliasValueClearsProperty() throws Exception {
        String wikiText = "@alias\n";
        InputStream in = new ByteArrayInputStream(wikiText.getBytes());

        TreeNode tree = parser.parse(in);

        assertNotNull("Tree should not be null", tree);
        assertTrue("Alias should be present (as empty string to signal clearing)", tree.alias.isPresent());
        assertEquals("Alias should be empty string", "", tree.alias.get());
    }

    @Test
    public void testEmptyTextValueClearsProperty() throws Exception {
        String wikiText = "* node\n@text\n";
        InputStream in = new ByteArrayInputStream(wikiText.getBytes());

        TreeNode tree = parser.parse(in);

        assertNotNull("Tree should not be null", tree);
        assertEquals("Root should have one child", 1, tree.children.size());

        TreeNode child = tree.children.get(0);
        assertTrue("Text should be present (as empty string to signal clearing)", child.text.isPresent());
        assertEquals("Text should be empty string", "", child.text.get());
    }

    @Test
    public void testEmptyPriorityValueClearsProperty() throws Exception {
        String wikiText = "* node\n@priority\n";
        InputStream in = new ByteArrayInputStream(wikiText.getBytes());

        TreeNode tree = parser.parse(in);

        assertNotNull("Tree should not be null", tree);
        assertEquals("Root should have one child", 1, tree.children.size());

        TreeNode child = tree.children.get(0);
        assertTrue("Priority should be present (with sentinel value to signal clearing)", child.priority.isPresent());
        assertTrue("Priority should be negative sentinel value", child.priority.get().value < 0.0f);
    }

    @Test
    public void testEmptyShortcutValueClearsProperty() throws Exception {
        String wikiText = "@shortcut\n";
        InputStream in = new ByteArrayInputStream(wikiText.getBytes());

        TreeNode tree = parser.parse(in);

        assertNotNull("Tree should not be null", tree);
        assertTrue("Shortcut should be present (as empty string to signal clearing)", tree.shortcut.isPresent());
        assertEquals("Shortcut should be empty string", "", tree.shortcut.get());
    }
}
