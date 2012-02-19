package net.fortytwo.myotherbrain.notes;

import junit.framework.TestCase;

import java.io.ByteArrayInputStream;
import java.io.IOException;
import java.io.InputStream;
import java.util.List;

/**
 * @author Joshua Shinavier (http://fortytwo.net)
 */
public class NotesSyntaxTest extends TestCase {
    private NotesSyntax syntax;

    @Override
    public void setUp() throws Exception {
        syntax = new NotesSyntax();
    }

    @Override
    public void tearDown() throws Exception {
    }

    public void testReadNotes() throws Exception {
        List<Note> notes = syntax.readNotes(NotesSyntax.class.getResourceAsStream("example-notes.txt"));
        assertEquals(6, notes.size());
        Note indentation = notes.get(1);
        assertNull(indentation.getTargetKey());
        assertEquals("indentation", indentation.getTargetValue());
        assertEquals("and this", indentation.getChildren()
                .get(2).getChildren()
                .get(0).getChildren()
                .get(0).getChildren()
                .get(0).getTargetValue());
        Note ws = notes.get(3);
        assertEquals(4, ws.getChildren().size());
        assertEquals("newlines can be preserved with triple braces {{{\n" +
                "like this.\n" +
                "Use as many lines of text as you need.\n" +
                "}}}", ws.getChildren().get(2).getTargetValue());
        Note ids = notes.get(4);
        assertEquals("ids", ids.getTargetValue());
        assertEquals("0txXBm", ids.getChildren().get(0).getTargetKey());
        assertEquals("cE85nD", ids.getChildren().get(1).getTargetKey());
    }

    private List<Note> readNotes(final String s) throws IOException, NotesSyntax.NoteParsingException {
        InputStream in = new ByteArrayInputStream(s.getBytes());
        try {
            return syntax.readNotes(in);
        } finally {
            in.close();
        }
    }
}
