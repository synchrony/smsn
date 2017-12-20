package net.fortytwo.smsn.brain.io.vcs;

import net.fortytwo.smsn.brain.model.dto.TopicDTO;
import net.fortytwo.smsn.brain.model.entities.Topic;
import org.junit.Test;

import java.io.File;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertFalse;
import static org.junit.Assert.assertTrue;

public class VCSFormatTest {
    @Test
    public void smsnFilesAreAccepted() throws Exception {
        assertTrue(VCSFormat.isDataFile(new File("UuYPT6ck7jBL6abZ.smsn")));
        assertTrue(VCSFormat.isDataFile(new File("0KOr3InLzs3sgAEK.smsn")));

        assertTrue(VCSFormat.isDataFile(new File("UuYPT.smsn")));
        File dir = new File("/tmp");
        assertTrue(VCSFormat.isDataFile(new File(dir, "UuYPT.smsn")));
    }

    @Test
    public void nonSmSnFilesRejected() throws Exception {
        assertFalse(VCSFormat.isDataFile(new File("1234.smsn")));
        assertFalse(VCSFormat.isDataFile(new File("?1234567.smsn")));
        assertFalse(VCSFormat.isDataFile(new File("12345678 9.smsn")));
        assertFalse(VCSFormat.isDataFile(new File("12345678")));
        assertFalse(VCSFormat.isDataFile(new File("a1234567.txt")));
    }

    @Test
    public void oldSchoolSmSnFilesRejected() {
        assertFalse(VCSFormat.isDataFile(new File("au-lH78_")));
        File dir = new File("/tmp");
        assertFalse(VCSFormat.isDataFile(new File(dir, "a1_234567")));
    }

    @Test
    public void topicsMapToExpectedFileNames() {
        assertEquals("12345.smsn", VCSFormat.fileNameForTopic(createTopic("12345")));
        assertEquals("abcdefgh.smsn", VCSFormat.fileNameForTopic(createTopic("abcdefgh")));
    }

    private Topic createTopic(final String id) {
        Topic topic = new TopicDTO();
        topic.setId(id);
        return topic;
    }
}
