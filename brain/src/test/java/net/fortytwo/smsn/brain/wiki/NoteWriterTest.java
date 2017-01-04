package net.fortytwo.smsn.brain.wiki;

import net.fortytwo.smsn.SemanticSynchrony;
import net.fortytwo.smsn.brain.model.Note;
import org.json.JSONArray;
import org.json.JSONObject;
import org.junit.Before;
import org.junit.Test;

import java.io.ByteArrayOutputStream;
import java.io.IOException;
import java.util.List;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertFalse;
import static org.junit.Assert.assertNull;
import static org.junit.Assert.assertTrue;

public class NoteWriterTest {
    private NoteReader parser;
    private NoteWriter writer;

    @Before
    public void setUp() {
        parser = new NoteReader();
        writer = new NoteWriter();
    }

    @Test
    public void jsonOutputIsNormal() throws Exception {
        Note n = parser.fromWikiText("" +
                "* foo\n" +
                "   * bar\n" +
                "   * quux\n");

        JSONObject j = writer.toJSON(n);

        assertTrue(j.getBoolean(NoteWriter.HAS_CHILDREN));
        JSONArray c = j.getJSONArray(NoteWriter.CHILDREN);
        assertEquals(1, c.length());

        JSONObject n1 = c.getJSONObject(0);
        assertTrue(n1.getBoolean(NoteWriter.HAS_CHILDREN));
        assertEquals("foo", n1.getString(SemanticSynchrony.VALUE));
        JSONArray c1 = n1.getJSONArray(NoteWriter.CHILDREN);
        assertEquals(2, c1.length());

        JSONObject n2 = c1.getJSONObject(0);
        assertFalse(n2.getBoolean(NoteWriter.HAS_CHILDREN));
        assertEquals("bar", n2.getString(SemanticSynchrony.VALUE));
        assertNull(n2.optJSONArray(NoteWriter.CHILDREN));

        JSONObject n3 = c1.getJSONObject(1);
        assertFalse(n3.getBoolean(NoteWriter.HAS_CHILDREN));
        assertEquals("quux", n3.getString(SemanticSynchrony.VALUE));
        assertNull(n3.optJSONArray(NoteWriter.CHILDREN));
    }

    @Test
    public void longValuesAreTruncated() throws Exception {
        Note n = parser.fromWikiText("" +
                "* this is a long line (well, not really)\n");

        int before = writer.getValueLengthCutoff();
        try {
            writer.setValueLengthCutoff(10);

            JSONObject j = writer.toJSON(n);

            assertEquals("this is a  [...]",
                    j.getJSONArray(NoteWriter.CHILDREN).getJSONObject(0).getString(SemanticSynchrony.VALUE));
        } finally {
            writer.setValueLengthCutoff(before);
        }
    }

    @Test
    public void verbatimBlocksArePreserved() throws Exception {
        String text = "* {{{\n"
                + "this\n"
                + "  is a\n"
                + "verbatim block\n" +
                "}}}";
        List<Note> notes = parser.fromWikiText(text).getChildren();
        Note n = notes.get(0);

        String value = n.getValue();
        String[] lines = value.split("\\n");
        assertEquals(3, lines.length);
        assertEquals("this", lines[0]);
        assertEquals("  is a", lines[1]);
        assertEquals("verbatim block", lines[2]);

        assertEquals(text + "\n", toWikiText(notes));
    }

    private String toWikiText(final List<Note> notes) throws IOException {
        try (ByteArrayOutputStream outputStream = new ByteArrayOutputStream()) {
            writer.toWikiText(notes, outputStream, false);
            return outputStream.toString();
        }
    }

    /*
    @Test
    public void testHideNonSharable() throws Exception {
        List<Note> notes = parser.parse(NoteParser.class.getResourceAsStream("wiki-example-3.txt")).getChildren();
        writer.writeNotes(notes, System.out);
    }*/
}
