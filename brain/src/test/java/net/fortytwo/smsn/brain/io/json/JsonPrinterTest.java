package net.fortytwo.smsn.brain.io.json;

import net.fortytwo.smsn.SemanticSynchrony;
import net.fortytwo.smsn.brain.io.wiki.WikiParser;
import net.fortytwo.smsn.brain.model.dto.TreeNodeDTO;
import net.fortytwo.smsn.brain.model.entities.Link;
import net.fortytwo.smsn.brain.model.entities.TreeNode;
import net.fortytwo.smsn.brain.query.TreeViews;
import org.json.JSONArray;
import org.json.JSONObject;
import org.junit.Before;
import org.junit.Test;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertNull;

public class JsonPrinterTest {
    private WikiParser wikiParser;
    private JsonPrinter jsonPrinter;

    @Before
    public void setUp() {
        wikiParser =  new WikiParser();
        jsonPrinter = new JsonPrinter();
    }

    @Test
    public void jsonOutputIsNormal() throws Exception {
        TreeNode<Link> tree = wikiParser.parse("" +
                "* foo\n" +
                "   * bar\n" +
                "   * quux\n").getContent();

        JSONObject j = jsonPrinter.toJson(tree);

        assertEquals(0, j.getInt(JsonFormat.Keys.NUMBER_OF_CHILDREN));
        assertEquals(0, j.getInt(JsonFormat.Keys.NUMBER_OF_PARENTS));
        JSONArray c = j.getJSONArray(JsonFormat.Keys.CHILDREN);
        assertEquals(1, c.length());

        JSONObject n1 = c.getJSONObject(0);
        assertEquals(0, n1.getInt(JsonFormat.Keys.NUMBER_OF_CHILDREN));
        assertEquals(0, n1.getInt(JsonFormat.Keys.NUMBER_OF_PARENTS));
        assertEquals("foo", n1.getString(SemanticSynchrony.PropertyKeys.TITLE));
        JSONArray c1 = n1.getJSONArray(JsonFormat.Keys.CHILDREN);
        assertEquals(2, c1.length());

        JSONObject n2 = c1.getJSONObject(0);
        assertEquals(0, n2.getInt(JsonFormat.Keys.NUMBER_OF_CHILDREN));
        assertEquals(0, n2.getInt(JsonFormat.Keys.NUMBER_OF_PARENTS));
        assertEquals("bar", n2.getString(SemanticSynchrony.PropertyKeys.TITLE));
        assertNull(n2.optJSONArray(JsonFormat.Keys.CHILDREN));

        JSONObject n3 = c1.getJSONObject(1);
        assertEquals(0, n3.getInt(JsonFormat.Keys.NUMBER_OF_CHILDREN));
        assertEquals(0, n3.getInt(JsonFormat.Keys.NUMBER_OF_PARENTS));
        assertEquals("quux", n3.getString(SemanticSynchrony.PropertyKeys.TITLE));
        assertNull(n3.optJSONArray(JsonFormat.Keys.CHILDREN));
    }

    @Test
    public void longValuesAreTruncated() throws Exception {
        TreeNode<Link> n = wikiParser.parse("" +
                "* this is a long line (well, not really)\n").getContent();

        int before = jsonPrinter.getTitleLengthCutoff();
        try {
            jsonPrinter.setTitleLengthCutoff(10);

            JSONObject j = jsonPrinter.toJson(n);

            assertEquals("this is a  [...]",
                    j.getJSONArray(JsonFormat.Keys.CHILDREN).getJSONObject(0).getString(SemanticSynchrony.PropertyKeys.TITLE));
        } finally {
            jsonPrinter.setTitleLengthCutoff(before);
        }
    }

    @Test
    public void noTextGivesNoTextAttribute() throws Exception {
        TreeNode<Link> n = TreeNodeDTO.createEmptyNode();
        TreeViews.setTitle(n, "Arthur Dent");

        JSONObject j = jsonPrinter.toJson(n);
        assertEquals("Arthur Dent", j.getString(SemanticSynchrony.PropertyKeys.TITLE));
        assertNull(j.opt(SemanticSynchrony.PropertyKeys.TEXT));
    }

    @Test
    public void textGivesTextAttribute() throws Exception {
        TreeNode<Link> n = TreeNodeDTO.createEmptyNode();
        TreeViews.setTitle(n, "Arthur Dent");
        TreeViews.setText(n, "12345");

        JSONObject j = jsonPrinter.toJson(n);
        assertEquals("Arthur Dent", j.getString(SemanticSynchrony.PropertyKeys.TITLE));
        assertEquals("12345", j.getString(SemanticSynchrony.PropertyKeys.TEXT));
    }
}
