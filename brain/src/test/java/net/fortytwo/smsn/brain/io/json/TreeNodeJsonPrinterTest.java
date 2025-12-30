package net.fortytwo.smsn.brain.io.json;

import net.fortytwo.smsn.SemanticSynchrony;
import net.fortytwo.smsn.brain.AtomId;
import net.fortytwo.smsn.brain.Normed;
import net.fortytwo.smsn.brain.SourceName;
import net.fortytwo.smsn.brain.Timestamp;
import net.fortytwo.smsn.brain.TreeNode;
import org.json.JSONArray;
import org.json.JSONObject;
import org.junit.Before;
import org.junit.Test;

import java.io.IOException;
import java.util.Arrays;
import java.util.Collections;

import static org.junit.Assert.*;

/**
 * Tests for TreeNodeJsonPrinter - JSON serialization of TreeNode objects.
 */
public class TreeNodeJsonPrinterTest {
    private TreeNodeJsonPrinter printer;

    @Before
    public void setUp() {
        printer = new TreeNodeJsonPrinter();
    }

    @Test
    public void serializesRequiredFields() throws IOException {
        TreeNode node = createNode("test-id", "Test Title");

        JSONObject json = printer.toJson(node);

        assertEquals("test-id", json.getString(JsonFormat.Keys.ID));
        assertEquals("Test Title", json.getString(SemanticSynchrony.PropertyKeys.TITLE));
        assertEquals("private", json.getString(SemanticSynchrony.PropertyKeys.SOURCE));
        assertTrue(json.has(SemanticSynchrony.PropertyKeys.CREATED));
        assertTrue(json.has(SemanticSynchrony.PropertyKeys.WEIGHT));
    }

    @Test
    public void serializesWeight() throws IOException {
        TreeNode node = createNode("id", "Title").withWeight(new Normed(0.75f));

        JSONObject json = printer.toJson(node);

        assertEquals(0.75f, (float) json.getDouble(SemanticSynchrony.PropertyKeys.WEIGHT), 0.001f);
    }

    @Test
    public void serializesTimestamp() throws IOException {
        long time = 1234567890000L;
        TreeNode node = createNode("id", "Title").withCreated(new Timestamp(time));

        JSONObject json = printer.toJson(node);

        assertEquals(time, json.getLong(SemanticSynchrony.PropertyKeys.CREATED));
    }

    @Test
    public void serializesNumberOfChildren() throws IOException {
        TreeNode node = createNode("id", "Title").withNumberOfChildren(5);

        JSONObject json = printer.toJson(node);

        assertEquals(5, json.getInt(JsonFormat.Keys.NUMBER_OF_CHILDREN));
    }

    @Test
    public void serializesNumberOfParents() throws IOException {
        TreeNode node = createNode("id", "Title").withNumberOfParents(3);

        JSONObject json = printer.toJson(node);

        assertEquals(3, json.getInt(JsonFormat.Keys.NUMBER_OF_PARENTS));
    }

    @Test
    public void serializesPriority() throws IOException {
        TreeNode node = createNode("id", "Title")
            .withPriority(hydra.util.Opt.of(new Normed(0.9f)));

        JSONObject json = printer.toJson(node);

        assertTrue(json.has(SemanticSynchrony.PropertyKeys.PRIORITY));
        assertEquals(0.9f, (float) json.getDouble(SemanticSynchrony.PropertyKeys.PRIORITY), 0.001f);
    }

    @Test
    public void omitsPriorityWhenEmpty() throws IOException {
        TreeNode node = createNode("id", "Title");

        JSONObject json = printer.toJson(node);

        assertFalse(json.has(SemanticSynchrony.PropertyKeys.PRIORITY));
    }

    @Test
    public void serializesAlias() throws IOException {
        TreeNode node = createNode("id", "Title")
            .withAlias(hydra.util.Opt.of("test-alias"));

        JSONObject json = printer.toJson(node);

        assertEquals("test-alias", json.getString(SemanticSynchrony.PropertyKeys.ALIAS));
    }

    @Test
    public void omitsAliasWhenEmpty() throws IOException {
        TreeNode node = createNode("id", "Title");

        JSONObject json = printer.toJson(node);

        assertFalse(json.has(SemanticSynchrony.PropertyKeys.ALIAS));
    }

    @Test
    public void serializesText() throws IOException {
        TreeNode node = createNode("id", "Title")
            .withText(hydra.util.Opt.of("Extended text content here"));

        JSONObject json = printer.toJson(node);

        assertEquals("Extended text content here", json.getString(SemanticSynchrony.PropertyKeys.TEXT));
    }

    @Test
    public void omitsTextWhenEmpty() throws IOException {
        TreeNode node = createNode("id", "Title");

        JSONObject json = printer.toJson(node);

        assertFalse(json.has(SemanticSynchrony.PropertyKeys.TEXT));
    }

    @Test
    public void serializesShortcut() throws IOException {
        TreeNode node = createNode("id", "Title")
            .withShortcut(hydra.util.Opt.of("ctrl+k"));

        JSONObject json = printer.toJson(node);

        assertEquals("ctrl+k", json.getString(SemanticSynchrony.PropertyKeys.SHORTCUT));
    }

    @Test
    public void omitsShortcutWhenEmpty() throws IOException {
        TreeNode node = createNode("id", "Title");

        JSONObject json = printer.toJson(node);

        assertFalse(json.has(SemanticSynchrony.PropertyKeys.SHORTCUT));
    }

    @Test
    public void serializesChildren() throws IOException {
        TreeNode child1 = createNode("child1", "Child 1");
        TreeNode child2 = createNode("child2", "Child 2");
        TreeNode parent = createNode("parent", "Parent")
            .withChildren(Arrays.asList(child1, child2));

        JSONObject json = printer.toJson(parent);

        assertTrue(json.has(JsonFormat.Keys.CHILDREN));
        JSONArray children = json.getJSONArray(JsonFormat.Keys.CHILDREN);
        assertEquals(2, children.length());
        assertEquals("child1", children.getJSONObject(0).getString(JsonFormat.Keys.ID));
        assertEquals("child2", children.getJSONObject(1).getString(JsonFormat.Keys.ID));
    }

    @Test
    public void omitsChildrenWhenEmpty() throws IOException {
        TreeNode node = createNode("id", "Title").withChildren(Collections.emptyList());

        JSONObject json = printer.toJson(node);

        assertFalse(json.has(JsonFormat.Keys.CHILDREN));
    }

    @Test
    public void defaultNodeHasNoChildrenInJson() throws IOException {
        // Default node has empty children list, which should not appear in JSON
        TreeNode node = createNode("id", "Title");

        JSONObject json = printer.toJson(node);

        assertFalse(json.has(JsonFormat.Keys.CHILDREN));
    }

    @Test
    public void serializesNestedChildren() throws IOException {
        TreeNode grandchild = createNode("grandchild", "Grandchild");
        TreeNode child = createNode("child", "Child")
            .withChildren(Collections.singletonList(grandchild));
        TreeNode parent = createNode("parent", "Parent")
            .withChildren(Collections.singletonList(child));

        JSONObject json = printer.toJson(parent);

        JSONArray children = json.getJSONArray(JsonFormat.Keys.CHILDREN);
        JSONObject childJson = children.getJSONObject(0);
        JSONArray grandchildren = childJson.getJSONArray(JsonFormat.Keys.CHILDREN);
        assertEquals("grandchild", grandchildren.getJSONObject(0).getString(JsonFormat.Keys.ID));
    }

    @Test
    public void titleTruncationWithCutoff() throws IOException {
        printer.setTitleLengthCutoff(10);
        TreeNode node = createNode("id", "This is a very long title that should be truncated");

        JSONObject json = printer.toJson(node);

        String title = json.getString(SemanticSynchrony.PropertyKeys.TITLE);
        assertEquals(10 + JsonFormat.TITLE_TRUNCATOR.length(), title.length());
        assertTrue(title.endsWith(JsonFormat.TITLE_TRUNCATOR));
        assertTrue(title.startsWith("This is a "));
    }

    @Test
    public void titleNotTruncatedWhenUnderCutoff() throws IOException {
        printer.setTitleLengthCutoff(100);
        TreeNode node = createNode("id", "Short title");

        JSONObject json = printer.toJson(node);

        assertEquals("Short title", json.getString(SemanticSynchrony.PropertyKeys.TITLE));
    }

    @Test
    public void titleNotTruncatedWhenCutoffDisabled() throws IOException {
        printer.setTitleLengthCutoff(-1);  // Disabled
        TreeNode node = createNode("id", "This is a very long title that should NOT be truncated");

        JSONObject json = printer.toJson(node);

        assertEquals("This is a very long title that should NOT be truncated",
            json.getString(SemanticSynchrony.PropertyKeys.TITLE));
    }

    @Test
    public void toJsonStringReturnsPrettyPrinted() throws IOException {
        TreeNode node = createNode("id", "Title");

        String jsonStr = printer.toJsonString(node);

        assertTrue("Should be pretty printed with newlines", jsonStr.contains("\n"));
        assertTrue("Should be pretty printed with indentation", jsonStr.contains("  "));
    }

    @Test
    public void getTitleLengthCutoffReturnsConfiguredValue() {
        printer.setTitleLengthCutoff(50);
        assertEquals(50, printer.getTitleLengthCutoff());
    }

    private TreeNode createNode(String id, String title) {
        return new TreeNode(
            new AtomId(id),
            new Timestamp(System.currentTimeMillis()),
            new Normed(0.5f),
            hydra.util.Opt.empty(),  // priority
            new SourceName("private"),
            title,
            hydra.util.Opt.empty(),  // alias
            hydra.util.Opt.empty(),  // text
            hydra.util.Opt.empty(),  // shortcut
            java.util.Collections.emptyList(),  // children
            0,  // numberOfChildren
            0   // numberOfParents
        );
    }
}
