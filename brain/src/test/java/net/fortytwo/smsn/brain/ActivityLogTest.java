package net.fortytwo.smsn.brain;

import org.junit.Before;
import org.junit.Test;

import java.io.File;
import java.io.FileWriter;
import java.io.IOException;
import java.nio.file.Files;

import static org.junit.Assert.*;

/**
 * Tests for ActivityLog - the activity logging system.
 */
public class ActivityLogTest {
    private File tempFile;
    private ActivityLog activityLog;

    @Before
    public void setUp() throws IOException {
        tempFile = File.createTempFile("activity-log-test", ".log");
        tempFile.deleteOnExit();
        activityLog = new ActivityLog(new FileWriter(tempFile));
    }

    @Test
    public void logCreateWritesEntry() throws IOException {
        Atom atom = createTestAtom("test-id-1", "Test Title");
        activityLog.logCreate(atom);
        activityLog.flush();

        String content = readLogContent();
        assertTrue("Log should contain create action", content.contains("\tcreate\t"));
        assertTrue("Log should contain atom id", content.contains("test-id-1"));
    }

    @Test
    public void logCreateByIdWritesEntry() throws IOException {
        AtomId atomId = new AtomId("test-id-2");
        activityLog.logCreateById(atomId);
        activityLog.flush();

        String content = readLogContent();
        assertTrue("Log should contain create action", content.contains("\tcreate\t"));
        assertTrue("Log should contain atom id", content.contains("test-id-2"));
    }

    @Test
    public void logViewWritesEntry() throws IOException {
        Atom atom = createTestAtom("view-id", "View Test");
        activityLog.logView(atom);
        activityLog.flush();

        String content = readLogContent();
        assertTrue("Log should contain view action", content.contains("\tview\t"));
        assertTrue("Log should contain atom id", content.contains("view-id"));
    }

    @Test
    public void logViewByIdWritesEntry() throws IOException {
        AtomId atomId = new AtomId("view-id-2");
        activityLog.logViewById(atomId);
        activityLog.flush();

        String content = readLogContent();
        assertTrue("Log should contain view action", content.contains("\tview\t"));
        assertTrue("Log should contain atom id", content.contains("view-id-2"));
    }

    @Test
    public void logSetPropertiesWritesEntry() throws IOException {
        Atom atom = createTestAtom("props-id", "Properties Test");
        activityLog.logSetProperties(atom);
        activityLog.flush();

        String content = readLogContent();
        assertTrue("Log should contain set-props action", content.contains("\tset-props\t"));
        assertTrue("Log should contain atom id", content.contains("props-id"));
    }

    @Test
    public void logSetPropertiesByIdWritesEntry() throws IOException {
        AtomId atomId = new AtomId("props-id-2");
        activityLog.logSetPropertiesById(atomId);
        activityLog.flush();

        String content = readLogContent();
        assertTrue("Log should contain set-props action", content.contains("\tset-props\t"));
        assertTrue("Log should contain atom id", content.contains("props-id-2"));
    }

    @Test
    public void logLinkWritesEntry() throws IOException {
        Atom tail = createTestAtom("tail-id", "Tail");
        Atom head = createTestAtom("head-id", "Head");
        activityLog.logLink(tail, head);
        activityLog.flush();

        String content = readLogContent();
        assertTrue("Log should contain link action", content.contains("\tlink\t"));
        assertTrue("Log should contain tail id", content.contains("tail-id"));
        assertTrue("Log should contain head id", content.contains("head-id"));
    }

    @Test
    public void logLinkByIdWritesEntry() throws IOException {
        AtomId tailId = new AtomId("tail-id-2");
        AtomId headId = new AtomId("head-id-2");
        activityLog.logLinkById(tailId, headId);
        activityLog.flush();

        String content = readLogContent();
        assertTrue("Log should contain link action", content.contains("\tlink\t"));
        assertTrue("Log should contain tail id", content.contains("tail-id-2"));
        assertTrue("Log should contain head id", content.contains("head-id-2"));
    }

    @Test
    public void logUnlinkWritesEntry() throws IOException {
        Atom tail = createTestAtom("unlink-tail", "Tail");
        Atom head = createTestAtom("unlink-head", "Head");
        activityLog.logUnlink(tail, head);
        activityLog.flush();

        String content = readLogContent();
        assertTrue("Log should contain unlink action", content.contains("\tunlink\t"));
        assertTrue("Log should contain tail id", content.contains("unlink-tail"));
        assertTrue("Log should contain head id", content.contains("unlink-head"));
    }

    @Test
    public void logUnlinkByIdWritesEntry() throws IOException {
        AtomId tailId = new AtomId("unlink-tail-2");
        AtomId headId = new AtomId("unlink-head-2");
        activityLog.logUnlinkById(tailId, headId);
        activityLog.flush();

        String content = readLogContent();
        assertTrue("Log should contain unlink action", content.contains("\tunlink\t"));
        assertTrue("Log should contain tail id", content.contains("unlink-tail-2"));
        assertTrue("Log should contain head id", content.contains("unlink-head-2"));
    }

    @Test
    public void logEntriesContainTimestamp() throws IOException {
        long before = System.currentTimeMillis();
        activityLog.logCreateById(new AtomId("timestamp-test"));
        long after = System.currentTimeMillis();
        activityLog.flush();

        String content = readLogContent();
        // Parse out the timestamp (first field before first tab)
        String[] parts = content.trim().split("\t");
        assertTrue("Should have at least 2 parts", parts.length >= 2);

        long timestamp = Long.parseLong(parts[0]);
        assertTrue("Timestamp should be >= before", timestamp >= before);
        assertTrue("Timestamp should be <= after", timestamp <= after);
    }

    @Test
    public void multipleLogEntriesWriteMultipleLines() throws IOException {
        activityLog.logCreateById(new AtomId("id1"));
        activityLog.logViewById(new AtomId("id2"));
        activityLog.logSetPropertiesById(new AtomId("id3"));
        activityLog.flush();

        String content = readLogContent();
        String[] lines = content.trim().split("\n");
        assertEquals("Should have 3 log entries", 3, lines.length);
    }

    @Test
    public void shutDownClosesWriter() throws IOException {
        activityLog.logCreateById(new AtomId("final-id"));
        activityLog.shutDown();

        // Verify file was written and closed
        String content = readLogContent();
        assertTrue("Content should be written before close", content.contains("final-id"));
    }

    private Atom createTestAtom(String id, String title) {
        return new Atom(
            new AtomId(id),
            new Timestamp(System.currentTimeMillis()),
            new Normed(0.5f),
            hydra.util.Opt.empty(),  // priority
            new SourceName("test"),
            title,
            hydra.util.Opt.empty(),  // alias
            hydra.util.Opt.empty(),  // text
            hydra.util.Opt.empty(),  // shortcut
            java.util.Collections.emptyList()  // children
        );
    }

    private String readLogContent() throws IOException {
        return Files.readString(tempFile.toPath());
    }
}
