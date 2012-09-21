package net.fortytwo.myotherbrain.notes;

import org.json.JSONArray;
import org.json.JSONObject;
import org.junit.Before;
import org.junit.Test;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertFalse;
import static org.junit.Assert.assertNull;
import static org.junit.Assert.assertTrue;

/**
 * @author Joshua Shinavier (http://fortytwo.net)
 */
public class NoteWriterTest {
    private NoteParser parser;
    private NoteWriter writer;

    @Before
    public void setUp() {
        parser = new NoteParser();
        writer = new NoteWriter();
    }

    @Test
    public void testNormal() throws Exception {
        Note n = parser.parse("" +
                "* foo\n" +
                "   * bar\n" +
                "   * quux\n");

        JSONObject j = writer.toJSON(n);

        //System.out.println(j);

        JSONObject t = j.getJSONObject(NoteWriter.TARGET);
        assertTrue(t.getBoolean(NoteWriter.HAS_CHILDREN));
        JSONArray c = j.getJSONArray(NoteWriter.CHILDREN);
        assertEquals(1, c.length());

        JSONObject n1 = c.getJSONObject(0);
        JSONObject t1 = n1.getJSONObject(NoteWriter.TARGET);
        assertTrue(t1.getBoolean(NoteWriter.HAS_CHILDREN));
        assertEquals("foo", t1.getString(NoteWriter.VALUE));
        JSONArray c1 = n1.getJSONArray(NoteWriter.CHILDREN);
        assertEquals(2, c1.length());

        JSONObject n2 = c1.getJSONObject(0);
        JSONObject t2 = n2.getJSONObject(NoteWriter.TARGET);
        assertFalse(t2.getBoolean(NoteWriter.HAS_CHILDREN));
        assertEquals("bar", t2.getString(NoteWriter.VALUE));
        assertNull(n2.optJSONArray(NoteWriter.CHILDREN));

        JSONObject n3 = c1.getJSONObject(1);
        JSONObject t3 = n3.getJSONObject(NoteWriter.TARGET);
        assertFalse(t3.getBoolean(NoteWriter.HAS_CHILDREN));
        assertEquals("quux", t3.getString(NoteWriter.VALUE));
        assertNull(n3.optJSONArray(NoteWriter.CHILDREN));
    }

    @Test
    public void testTruncateLongValues() throws Exception {
        Note n = parser.parse("" +
                "* this is a long line (well, not really)\n");

        int before = writer.getValueLengthCutoff();
        try {
            writer.setValueLengthCutoff(10);

            JSONObject j = writer.toJSON(n);

            assertEquals("this is a  [...]",
                    j.getJSONArray(NoteWriter.CHILDREN).getJSONObject(0).getJSONObject(NoteWriter.TARGET).getString(NoteWriter.VALUE));
        } finally {
            writer.setValueLengthCutoff(before);
        }
    }
}
