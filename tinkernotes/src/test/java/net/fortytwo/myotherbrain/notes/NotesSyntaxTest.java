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
        assertEquals(8, notes.size());
        Note indentation = notes.get(1);
        assertNull(indentation.getTargetKey());
        assertNull(indentation.getLinkKey());
        assertEquals("indentation", indentation.getTargetValue());
        assertEquals("and this", indentation.getChildren()
                .get(2).getChildren()
                .get(0).getChildren()
                .get(0).getChildren()
                .get(0).getTargetValue());
        Note n = notes.get(4).getChildren().get(2);
        assertEquals("xyz", n.getLinkKey());
        assertEquals("XYZ", n.getTargetKey());
        Note comments = notes.get(5);
        assertEquals("comments", comments.getTargetValue());
        n = notes.get(5).getChildren().get(1);
        assertEquals("abc", n.getLinkKey());
        assertEquals("def", n.getTargetKey());
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
