package net.fortytwo.smsn.brain.io.wiki;

import net.fortytwo.smsn.brain.model.Role;
import net.fortytwo.smsn.brain.model.dto.NoteDTO;
import net.fortytwo.smsn.brain.model.dto.TopicDTO;
import net.fortytwo.smsn.brain.model.entities.Note;
import net.fortytwo.smsn.brain.model.entities.Topic;
import org.junit.Before;
import org.junit.Test;

import java.io.ByteArrayOutputStream;

import static org.junit.Assert.assertEquals;

public class WikiPrinterTest {
    private WikiPrinter printer;
    private ByteArrayOutputStream outputStream;

    @Before
    public void setUp() {
        outputStream = new ByteArrayOutputStream();
        printer = new WikiPrinter(outputStream);
    }

    @Test
    public void propertiesPrecedeContent() throws Exception {
        Note note = createNote("123", "Arthur Dent", null);
        note.setAlias("http://example.org/ArthurDent");
        note.setPriority(1.0f);
        note.setShortcut("ad");

        assertEquals("@id 123\n" +
                "@title Arthur Dent\n" +
                "@alias http://example.org/ArthurDent\n"
                + "@shortcut ad\n"
                + "@priority 1.0\n", write(note));
    }

    @Test
    public void textFollowsContent() throws Exception {
        Note note = createNote("12345", "Arthur Dent", null);
        note.setText("The regular early morning yell of horror was the sound of Arthur Dent waking\n" +
                "up and suddenly remembering where he was.");

        assertEquals("@id 12345\n" +
                "@title Arthur Dent\n" +
                "@text ```\n" +
                "The regular early morning yell of horror was the sound of Arthur Dent waking\n" +
                "up and suddenly remembering where he was.\n" +
                "```\n", write(note));
    }

    @Test
    public void textIsOptional() throws Exception {
        Note note = createNote("12345", "Arthur Dent", null);
        note.setText(null);

        assertEquals("@id 12345\n" +
                "@title Arthur Dent\n", write(note));
    }

    @Test
    public void emptyTextIsIgnored() throws Exception {
        Note note = createNote("12345", "Arthur Dent", null);
        note.setText("  \n ");

        assertEquals("@id 12345\n" +
                "@title Arthur Dent\n", write(note));
    }

    @Test
    public void trailingTextWhitespaceIsIgnored() throws Exception {
        Note note = createNote("12345", "Arthur Dent", null);
        note.setText("one\ntwo\n\n  \t\n ");

        assertEquals("@id 12345\n" +
                "@title Arthur Dent\n" +
                "@text ```\n" +
                "one\n" +
                "two\n" +
                "```\n", write(note));
    }

    @Test
    public void nounAndVerbBulletsAreDistinguished() throws Exception {
        Note root = createNote("123", "Arthur Dent", null);
        Note node1 = createNote(null, "Arthur Philip Dent", null);
        Note node2 = createNote("12345", "friends", Role.Relation);
        Note node3 = createNote("00000", "Ford Prefect", null);
        Note node4 = createNote(null, "Slartibartfast", null);
        Note.setChildren(root, node1, node2);
        Note.setChildren(node2, node3, node4);

        assertEquals("@id 123\n" +
                "@title Arthur Dent\n" +
                "* Arthur Philip Dent\n" +
                "-- :12345: friends\n" +
                "    * :00000: Ford Prefect\n" +
                "    * Slartibartfast\n", write(root));
    }

    private Note createNote(final String id, final String label, final Role role) {
        Note note = new NoteDTO();
        note.setRole(role);
        note.setLabel(label);
        if (null != id) {
            Topic topic = new TopicDTO();
            topic.setId(id);
            note.setTopic(topic);
        }
        return note;
    }

    private String write(final Note note) {
        printer.print(note);
        return outputStream.toString();
    }
}
