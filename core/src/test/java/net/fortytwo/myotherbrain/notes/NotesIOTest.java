package net.fortytwo.myotherbrain.notes;

import junit.framework.TestCase;

import java.io.ByteArrayInputStream;
import java.io.IOException;
import java.io.InputStream;
import java.util.List;

/**
 * User: josh
 * Date: 6/20/11
 * Time: 7:58 PM
 */
public class NotesIOTest extends TestCase {
    //private Graph graph;
    //private FramesManager manager;
    private NotesIO io;

    @Override
    public void setUp() throws Exception {
        //graph = new TinkerGraph();
        //manager = new FramesManager(graph);
        io = new NotesIO();
    }

    @Override
    public void tearDown() throws Exception {
        //graph.shutdown();
    }

    public void testAll() throws Exception {
        String s = "" +
                ".  a\n" +
                "    .  b\n" +
                "    .  c\n" +
                "V  d\n" +
                "\n" +
                "[second context]\n" +
                ".  e";

        List<NoteContext> contexts = parse(s);
        assertEquals(2, contexts.size());
        assertEquals(2, contexts.get(0).getNotes().size());
        assertEquals(1, contexts.get(1).getNotes().size());
        assertEquals(2, contexts.get(0).getNotes().get(0).getChildren().size());
        assertEquals("a", contexts.get(0).getNotes().get(0).getText());
        assertEquals("c", contexts.get(0).getNotes().get(0).getChildren().get(1).getText());
        assertEquals("d", contexts.get(0).getNotes().get(1).getText());
        assertEquals("V", contexts.get(0).getNotes().get(1).getType());
        assertEquals("", contexts.get(0).getText());
        assertEquals("second context", contexts.get(1).getText());
        assertEquals("e", contexts.get(1).getNotes().get(0).getText());
    }

    private List<NoteContext> parse(final String s) throws IOException, NotesIO.NoteParsingException {
        InputStream in = new ByteArrayInputStream(s.getBytes());
        try {
            return io.parseContexts(in);
        } finally {
            in.close();
        }
    }
}
