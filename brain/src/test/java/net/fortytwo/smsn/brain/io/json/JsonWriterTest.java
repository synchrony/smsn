package net.fortytwo.smsn.brain.io.json;

import net.fortytwo.smsn.SemanticSynchrony;
import net.fortytwo.smsn.brain.io.wiki.WikiReader;
import net.fortytwo.smsn.brain.model.Note;
import org.json.JSONArray;
import org.json.JSONObject;
import org.junit.Before;
import org.junit.Test;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertNull;

public class JsonWriterTest {
    private WikiReader wikiReader;
    private JsonWriter jsonWriter;

    @Before
    public void setUp() {
        wikiReader = new WikiReader();
        jsonWriter = new JsonWriter();
    }

    @Test
    public void jsonOutputIsNormal() throws Exception {
        Note note = wikiReader.parse("" +
                "* foo\n" +
                "   * bar\n" +
                "   * quux\n");

        JSONObject j = jsonWriter.toJson(note);

        assertEquals(0, j.getInt(JsonFormat.NUMBER_OF_CHILDREN));
        assertEquals(0, j.getInt(JsonFormat.NUMBER_OF_PARENTS));
        JSONArray c = j.getJSONArray(JsonFormat.CHILDREN);
        assertEquals(1, c.length());

        JSONObject n1 = c.getJSONObject(0);
        assertEquals(0, n1.getInt(JsonFormat.NUMBER_OF_CHILDREN));
        assertEquals(0, n1.getInt(JsonFormat.NUMBER_OF_PARENTS));
        assertEquals("foo", n1.getString(SemanticSynchrony.VALUE));
        JSONArray c1 = n1.getJSONArray(JsonFormat.CHILDREN);
        assertEquals(2, c1.length());

        JSONObject n2 = c1.getJSONObject(0);
        assertEquals(0, n2.getInt(JsonFormat.NUMBER_OF_CHILDREN));
        assertEquals(0, n2.getInt(JsonFormat.NUMBER_OF_PARENTS));
        assertEquals("bar", n2.getString(SemanticSynchrony.VALUE));
        assertNull(n2.optJSONArray(JsonFormat.CHILDREN));

        JSONObject n3 = c1.getJSONObject(1);
        assertEquals(0, n3.getInt(JsonFormat.NUMBER_OF_CHILDREN));
        assertEquals(0, n3.getInt(JsonFormat.NUMBER_OF_PARENTS));
        assertEquals("quux", n3.getString(SemanticSynchrony.VALUE));
        assertNull(n3.optJSONArray(JsonFormat.CHILDREN));
    }

    @Test
    public void longValuesAreTruncated() throws Exception {
        Note n = wikiReader.parse("" +
                "* this is a long line (well, not really)\n");

        int before = jsonWriter.getValueLengthCutoff();
        try {
            jsonWriter.setValueLengthCutoff(10);

            JSONObject j = jsonWriter.toJson(n);

            assertEquals("this is a  [...]",
                    j.getJSONArray(JsonFormat.CHILDREN).getJSONObject(0).getString(SemanticSynchrony.VALUE));
        } finally {
            jsonWriter.setValueLengthCutoff(before);
        }
    }
}
