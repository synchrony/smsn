package net.fortytwo.smsn.brain.io.wiki;

import net.fortytwo.smsn.brain.model.Note;
import org.junit.Before;
import org.junit.Test;

import java.io.ByteArrayOutputStream;
import java.io.IOException;

import static org.junit.Assert.assertEquals;

public class WikiPrinterTest {
    private WikiPrinter wikiPrinter;

    @Before
    public void setUp() {
        wikiPrinter = new WikiPrinter();
    }

    @Test
    public void noPageUnlessUsingCanonicalFormat() throws IOException {
        wikiPrinter.setUseCanonicalFormat(false);

        Note note = new Note();
        note.setId("123");
        note.setTitle("Arthur Dent");
        note.setPage("He's a jerk.\nA complete kneebiter.");

        assertEquals("* :123: Arthur Dent\n", toWikiText(note));
    }

    @Test
    public void propertiesFollowTitle() throws Exception {
        Note note = new Note();
        note.setId("123");
        note.setTitle("Arthur Dent");
        note.setAlias("http://example.org/ArthurDent");

        assertEquals("* :123: Arthur Dent\n" +
                "    @alias http://example.org/ArthurDent\n", toWikiText(note));
    }

    @Test
    public void pageFollowsChildrenWhichFollowProperties() throws Exception {
        wikiPrinter.setUseCanonicalFormat(true);

        Note note = new Note();
        note.setId("123");
        note.setTitle("Arthur Dent");
        note.setAlias("http://example.org/ArthurDent");
        note.setPage("He's a jerk.\nA complete kneebiter.");
        Note child = new Note();
        child.setId("abc");
        note.addChild(child);

        assertEquals("* :123: Arthur Dent\n" +
                "    @alias http://example.org/ArthurDent\n" +
                "    * :abc: \n" +
                "\n" +
                "He's a jerk.\n" +
                "A complete kneebiter.", toWikiText(note));
    }

    @Test
    public void pageIsOptional() throws Exception {
        wikiPrinter.setUseCanonicalFormat(true);

        Note note = new Note();
        note.setId("123");
        note.setTitle("Arthur Dent");
        note.setPage("");

        assertEquals("* :123: Arthur Dent\n", toWikiText(note));
    }

    @Test
    public void emptyPageIsIgnored() throws Exception {
        wikiPrinter.setUseCanonicalFormat(true);

        Note note = new Note();
        note.setId("123");
        note.setTitle("Arthur Dent");
        note.setPage("  \n ");

        assertEquals("* :123: Arthur Dent\n", toWikiText(note));
    }

    @Test
    public void inlinePagesAreSupported() throws Exception {
        wikiPrinter.setUseCanonicalFormat(true);

        Note note = new Note();
        note.setTitle("Life, the Universe, and Everything");
        note.setPage("The regular early morning yell of horror was the sound of Arthur Dent waking\n" +
                "up and suddenly remembering where he was.");

        String text = "* Life, the Universe, and Everything\n" +
                "\n" +
                "The regular early morning yell of horror was the sound of Arthur Dent waking\n" +
                "up and suddenly remembering where he was.";

        assertEquals(text, toWikiText(note));
    }

    private String toWikiText(final Note note) throws IOException {
        try (ByteArrayOutputStream outputStream = new ByteArrayOutputStream()) {
            wikiPrinter.print(note, outputStream, true);
            return outputStream.toString();
        }
    }
}
