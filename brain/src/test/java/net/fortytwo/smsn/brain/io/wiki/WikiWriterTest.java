package net.fortytwo.smsn.brain.io.wiki;

import net.fortytwo.smsn.brain.io.wiki.WikiReader;
import net.fortytwo.smsn.brain.io.wiki.WikiWriter;
import net.fortytwo.smsn.brain.model.Note;
import org.junit.Before;
import org.junit.Test;

import java.io.ByteArrayOutputStream;
import java.io.IOException;
import java.util.List;

import static org.junit.Assert.assertEquals;

public class WikiWriterTest {
    private WikiReader wikiReader;
    private WikiWriter wikiWriter;

    @Before
    public void setUp() {
        wikiReader = new WikiReader();
        wikiWriter = new WikiWriter();
    }

    @Test
    public void verbatimBlocksArePreserved() throws Exception {
        String text = "* {{{\n"
                + "this\n"
                + "  is a\n"
                + "verbatim block\n" +
                "}}}";
        List<Note> notes = wikiReader.parse(text).getChildren();
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
            wikiWriter.toWikiText(notes, outputStream, false);
            return outputStream.toString();
        }
    }
}
