package net.fortytwo.smsn.brain.io.json;

import net.fortytwo.smsn.SemanticSynchrony;
import net.fortytwo.smsn.brain.TreeNode;
import org.junit.Before;
import org.junit.Test;

import java.io.ByteArrayInputStream;
import java.io.IOException;
import java.nio.charset.StandardCharsets;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertFalse;
import static org.junit.Assert.assertNotNull;
import static org.junit.Assert.assertTrue;

public class TreeNodeJsonParserTest {

    private TreeNodeJsonParser parser;

    @Before
    public void setUp() {
        parser = new TreeNodeJsonParser();
    }

    @Test
    public void parsesMinimalNode() throws IOException {
        String json = "{}";
        TreeNode node = parser.parse(json);

        assertNotNull(node);
        assertNotNull(node.id);  // Should have temp ID
        assertTrue(node.id.value.startsWith("temp-"));
        assertEquals("", node.title);
        assertEquals(0.5f, node.weight.value, 0.001);
        assertEquals("public", node.source.value);
        assertTrue(node.children.isEmpty());
    }

    @Test
    public void parsesAllRequiredFields() throws IOException {
        String json = "{" +
                "\"" + JsonFormat.Keys.ID + "\": \"abc123\"," +
                "\"" + SemanticSynchrony.PropertyKeys.TITLE + "\": \"Test Title\"," +
                "\"" + SemanticSynchrony.PropertyKeys.CREATED + "\": 1609459200000," +
                "\"" + SemanticSynchrony.PropertyKeys.WEIGHT + "\": 0.75," +
                "\"" + SemanticSynchrony.PropertyKeys.SOURCE + "\": \"private\"" +
                "}";

        TreeNode node = parser.parse(json);

        assertEquals("abc123", node.id.value);
        assertEquals("Test Title", node.title);
        assertEquals(1609459200000L, (long) node.created.value);
        assertEquals(0.75f, node.weight.value, 0.001);
        assertEquals("private", node.source.value);
    }

    @Test
    public void parsesOptionalFields() throws IOException {
        String json = "{" +
                "\"" + JsonFormat.Keys.ID + "\": \"abc123\"," +
                "\"" + SemanticSynchrony.PropertyKeys.PRIORITY + "\": 0.9," +
                "\"" + SemanticSynchrony.PropertyKeys.ALIAS + "\": \"test-alias\"," +
                "\"" + SemanticSynchrony.PropertyKeys.TEXT + "\": \"Some extended text\"," +
                "\"" + SemanticSynchrony.PropertyKeys.SHORTCUT + "\": \"t\"" +
                "}";

        TreeNode node = parser.parse(json);

        assertTrue(node.priority.isPresent());
        assertEquals(0.9f, node.priority.get().value, 0.001);
        assertTrue(node.alias.isPresent());
        assertEquals("test-alias", node.alias.get());
        assertTrue(node.text.isPresent());
        assertEquals("Some extended text", node.text.get());
        assertTrue(node.shortcut.isPresent());
        assertEquals("t", node.shortcut.get());
    }

    @Test
    public void missingOptionalFieldsAreEmpty() throws IOException {
        String json = "{\"" + JsonFormat.Keys.ID + "\": \"abc123\"}";
        TreeNode node = parser.parse(json);

        assertFalse(node.priority.isPresent());
        assertFalse(node.alias.isPresent());
        assertFalse(node.text.isPresent());
        assertFalse(node.shortcut.isPresent());
    }

    @Test
    public void parsesChildren() throws IOException {
        String json = "{" +
                "\"" + JsonFormat.Keys.ID + "\": \"parent\"," +
                "\"" + SemanticSynchrony.PropertyKeys.TITLE + "\": \"Parent\"," +
                "\"" + JsonFormat.Keys.CHILDREN + "\": [" +
                "  {\"" + JsonFormat.Keys.ID + "\": \"child1\", \"" + SemanticSynchrony.PropertyKeys.TITLE + "\": \"Child 1\"}," +
                "  {\"" + JsonFormat.Keys.ID + "\": \"child2\", \"" + SemanticSynchrony.PropertyKeys.TITLE + "\": \"Child 2\"}" +
                "]" +
                "}";

        TreeNode node = parser.parse(json);

        assertEquals("parent", node.id.value);
        assertEquals(2, node.children.size());
        assertEquals("child1", node.children.get(0).id.value);
        assertEquals("Child 1", node.children.get(0).title);
        assertEquals("child2", node.children.get(1).id.value);
        assertEquals("Child 2", node.children.get(1).title);
    }

    @Test
    public void parsesNestedChildren() throws IOException {
        String json = "{" +
                "\"" + JsonFormat.Keys.ID + "\": \"root\"," +
                "\"" + JsonFormat.Keys.CHILDREN + "\": [" +
                "  {" +
                "    \"" + JsonFormat.Keys.ID + "\": \"level1\"," +
                "    \"" + JsonFormat.Keys.CHILDREN + "\": [" +
                "      {\"" + JsonFormat.Keys.ID + "\": \"level2\"}" +
                "    ]" +
                "  }" +
                "]" +
                "}";

        TreeNode node = parser.parse(json);

        assertEquals("root", node.id.value);
        assertEquals(1, node.children.size());
        TreeNode level1 = node.children.get(0);
        assertEquals("level1", level1.id.value);
        assertEquals(1, level1.children.size());
        assertEquals("level2", level1.children.get(0).id.value);
    }

    @Test
    public void parsesNumberOfChildrenAndParents() throws IOException {
        String json = "{" +
                "\"" + JsonFormat.Keys.ID + "\": \"abc123\"," +
                "\"" + JsonFormat.Keys.NUMBER_OF_CHILDREN + "\": 5," +
                "\"" + JsonFormat.Keys.NUMBER_OF_PARENTS + "\": 2" +
                "}";

        TreeNode node = parser.parse(json);

        assertEquals(5, (int) node.numberOfChildren);
        assertEquals(2, (int) node.numberOfParents);
    }

    @Test
    public void parsesFromInputStream() throws IOException {
        String json = "{\"" + JsonFormat.Keys.ID + "\": \"stream-id\", " +
                "\"" + SemanticSynchrony.PropertyKeys.TITLE + "\": \"Stream Test\"}";
        ByteArrayInputStream in = new ByteArrayInputStream(json.getBytes(StandardCharsets.UTF_8));

        TreeNode node = parser.parse(in);

        assertEquals("stream-id", node.id.value);
        assertEquals("Stream Test", node.title);
    }

    @Test(expected = IOException.class)
    public void throwsOnInvalidJson() throws IOException {
        parser.parse("{invalid json}");
    }

    @Test
    public void handlesEmptyChildrenArray() throws IOException {
        String json = "{" +
                "\"" + JsonFormat.Keys.ID + "\": \"abc123\"," +
                "\"" + JsonFormat.Keys.CHILDREN + "\": []" +
                "}";

        TreeNode node = parser.parse(json);

        assertNotNull(node.children);
        assertTrue(node.children.isEmpty());
    }
}
