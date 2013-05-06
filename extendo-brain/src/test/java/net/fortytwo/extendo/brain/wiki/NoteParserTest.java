package net.fortytwo.extendo.brain.wiki;

import net.fortytwo.extendo.brain.BrainGraph;
import net.fortytwo.extendo.brain.Note;
import org.junit.After;
import org.junit.Before;
import org.junit.Test;

import java.io.ByteArrayInputStream;
import java.io.IOException;
import java.io.InputStream;
import java.util.List;

import static junit.framework.Assert.assertEquals;
import static junit.framework.Assert.assertNull;

/**
 * @author Joshua Shinavier (http://fortytwo.net)
 */
public class NoteParserTest {
    private NoteParser parser;

    @Before
    public void setUp() throws Exception {
        parser = new NoteParser();
    }

    @After
    public void tearDown() throws Exception {
    }

    @Test
    public void testExample1() throws Exception {
        List<Note> notes = parser.parse(BrainGraph.class.getResourceAsStream("wiki-example-1.txt")).getChildren();
        assertEquals(7, notes.size());

        Note indentation = notes.get(1);
        assertNull(indentation.getId());
        assertEquals("indentation", indentation.getValue());
        assertEquals("and this", indentation.getChildren()
                .get(2).getChildren()
                .get(0).getChildren()
                .get(0).getChildren()
                .get(0).getValue());

        Note atts = notes.get(3);
        assertEquals("http://example.org/ns/attributes", atts.getAlias());
        assertEquals(0.75f, atts.getWeight());

        Note ws = notes.get(4);
        assertEquals(5, ws.getChildren().size());
        assertEquals("newlines can be preserved with triple braces {{{\n" +
                "like this.\n" +
                "Use as many lines of text as you need.\n" +
                "}}}", ws.getChildren().get(2).getValue());
        assertEquals("leading and trailing whitespace are ignored", ws.getChildren().get(3).getValue());

        Note ids = notes.get(5);
        assertEquals("ids", ids.getValue());
        assertEquals("0txXBm", ids.getChildren().get(0).getId());
        assertEquals("cE85nD", ids.getChildren().get(1).getId());
    }

    @Test
    public void testExample2() throws Exception {
        Note root = parser.parse(BrainGraph.class.getResourceAsStream("wiki-example-2.txt"));

        assertEquals("http://example.org/ns/top-level-attributes-are-allowed", root.getAlias());
        assertEquals(1.0f, root.getWeight());
        assertEquals(0.75f, root.getSharability());
        assertEquals(0.5f, root.getPriority());

        assertEquals(1, root.getChildren().size());
        assertEquals(1, root.getChildren().get(0).getChildren().size());
    }

    @Test
    public void testEmptyLinesIgnored() throws Exception {
        List<Note> notes = readNotes(
                "* one\n" +
                "   \n" +     // empty line with additional whitespace
                "* two" +
                "\n" +        // empty line without additional whitespace
                "* three");
        assertEquals(3, notes.size());
    }

    @Test(expected = NoteParser.NoteParsingException.class)
    public void testEmptyValuesNotAllowedForNewNotes() throws Exception {
        readNotes("* ");
    }

    @Test
    public void testEmptyValuesAllowedForExistingNotes() throws Exception {
        readNotes("* :1234567: ");
    }

    @Test
    public void testEmptyAliasAttributeAllowed() throws Exception {
        readNotes("@alias ");
    }

    @Test(expected = NoteParser.NoteParsingException.class)
    public void testEmptyPriorityAttributeNotAllowed() throws Exception {
        readNotes("@priority ");
    }

    @Test(expected = NoteParser.NoteParsingException.class)
    public void testEmptySharabilityAttributeNotAllowed() throws Exception {
        readNotes("@sharability ");
    }

    @Test(expected = NoteParser.NoteParsingException.class)
    public void testEmptyWeightAttributeNotAllowed() throws Exception {
        readNotes("@weight ");
    }

    @Test(expected = NoteParser.NoteParsingException.class)
    public void testLineTruncationSequenceNotAllowed() throws Exception {
        readNotes("" +
                "* this is a note whose value was truncated for readability [...]\n" +
                "   * you wouldn't want to lose the actual value because of a careless copy and paste, would you?");
    }

    @Test
    public void testVerbatimBlocks() throws Exception {
        List<Note> notes = readNotes("* here is a verbatim block {{{\n" +
                "which spans two lines}}}");

        notes = readNotes("* here is a verbatim block {{{ all in one line}}} (pointless, but permitted)");

        notes = readNotes("* :0001: here is a verbatim block {{{\n" +
                "with an id }}}");
        assertEquals(1, notes.size());
        assertEquals("0001", notes.get(0).getId());
    }

    private List<Note> readNotes(final String s) throws IOException, NoteParser.NoteParsingException {
        InputStream in = new ByteArrayInputStream(s.getBytes());
        try {
            return parser.parse(in).getChildren();
        } finally {
            in.close();
        }
    }
}
