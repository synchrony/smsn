package net.fortytwo.smsn.brain.io.vcs;

import net.fortytwo.smsn.brain.AtomId;
import net.fortytwo.smsn.brain.model.dto.TopicDTO;
import net.fortytwo.smsn.brain.model.entities.Topic;
import org.junit.Test;

import java.io.File;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertFalse;
import static org.junit.Assert.assertTrue;

public class VCSFormatTest {
    private final FilePerNoteFormat format = VCSWriter.FORMAT;

    @Test
    public void smsnFilesAreAccepted() {
        assertTrue(format.isMatchingFile(new File("UuYPT6ck7jBL6abZ.smsn")));
        File dir = new File("/tmp");
        assertTrue(format.isMatchingFile(new File(dir,"0KOr3InLzs3sgAEK.smsn")));
    }

    @Test
    public void nonSmSnFilesRejected() {
        assertFalse(format.isMatchingFile(new File("?1234567aaaaa.smsn")));
        assertFalse(format.isMatchingFile(new File("12345678 9aaa.smsn")));
        assertFalse(format.isMatchingFile(new File("12345678aaaaa")));
        assertFalse(format.isMatchingFile(new File("aaaaa1234567.txt")));

        // These file names are too short, though otherwise legal
        assertFalse(format.isMatchingFile(new File("1234.smsn")));
        assertFalse(format.isMatchingFile(new File("UuYPT.smsn")));
        File dir = new File("/tmp");
        assertFalse(format.isMatchingFile(new File(dir, "UuYPT.smsn")));
    }

    @Test
    public void oldSchoolSmSnFilesRejected() {
        assertFalse(format.isMatchingFile(new File("au-lH78_")));
        File dir = new File("/tmp");
        assertFalse(format.isMatchingFile(new File(dir, "a1_234567")));
    }

    @Test
    public void topicsMapToExpectedFileNames() {
        assertEquals("12345.smsn", format.fileNameFor(createTopic(new AtomId("12345"))));
        assertEquals("abcdefgh.smsn", format.fileNameFor(createTopic(new AtomId("abcdefgh"))));
    }

    private Topic createTopic(final AtomId id) {
        Topic topic = new TopicDTO();
        topic.setId(id);
        return topic;
    }
}
