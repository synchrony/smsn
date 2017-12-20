package net.fortytwo.smsn.brain.io.vcs;

import net.fortytwo.smsn.SemanticSynchrony;
import net.fortytwo.smsn.brain.BrainTestBase;
import net.fortytwo.smsn.brain.io.Format;
import net.fortytwo.smsn.brain.io.NoteWriter;
import net.fortytwo.smsn.brain.model.Filter;
import net.fortytwo.smsn.brain.model.Role;
import net.fortytwo.smsn.brain.model.TopicGraph;
import net.fortytwo.smsn.brain.model.entities.ListNode;
import net.fortytwo.smsn.brain.model.entities.Note;
import net.fortytwo.smsn.brain.model.entities.Topic;
import net.fortytwo.smsn.brain.query.ViewStyle;
import org.junit.Test;

import java.io.BufferedReader;
import java.io.File;
import java.io.FileInputStream;
import java.io.IOException;
import java.io.InputStream;
import java.io.InputStreamReader;
import java.util.HashMap;
import java.util.LinkedList;
import java.util.List;
import java.util.Map;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertTrue;

public class VCSWriterTest extends BrainTestBase {
    @Override
    protected TopicGraph createTopicGraph() throws IOException {
        return createTinkerTopicGraph();
    }

    @Test
    public void formatIsCorrect() {
        List<Format> formats = new VCSWriter().getFormats();
        assertEquals(1, formats.size());
        assertEquals("VCS", formats.get(0).getName());
    }

    @Test
    public void emptyGraphProducesEmptyDirectory() throws Exception {
        File dir = doExport();
        assertEquals(4, dir.listFiles().length);
        for (File subdir : dir.listFiles()) {
            assertTrue(subdir.isDirectory());
            assertEquals(0, subdir.listFiles().length);
        }
    }

    @Test
    public void singlePageIsWrittenCorrectly() throws Exception {
        String source = DefaultSources.PUBLIC;
        String fordId = SemanticSynchrony.createRandomId();
        String zaphodId = SemanticSynchrony.createRandomId();

        Note root = createNote("11111", "change me");
        root.setSource(source);

        Note note = createNoteDTO(root.getTopic().getId(), "Arthur Dent");
        note.setSource(source);
        note.setText("He's a jerk.\nA complete kneebiter.");
        Note fordTree = createNoteDTO(fordId, "Ford Prefect");
        Note zaphodTree = createNoteDTO(zaphodId, "Zaphod Beeblebrox");
        note.addChild(0, fordTree);
        note.addChild(1, zaphodTree);

        model.view()
                .root(root).height(Integer.MAX_VALUE).filter(Filter.noFilter()).style(ViewStyle.Basic.Forward.getStyle())
                .put(note);

        Topic adTopic = topicGraph.getTopicById(root.getTopic().getId()).get();
        Note ad = topicGraph.createNote(adTopic, "Arthur dent", Role.Entity);

        assertEquals(DefaultSources.PUBLIC, ad.getSource());
        assertEquals("Arthur Dent", ad.getLabel());
        assertEquals("He's a jerk.\nA complete kneebiter.", ad.getText());
        assertEquals(2, ListNode.toJavaList(ad.getFirst()).size());
        Note random = ad.getFirst();
        assertEquals("Ford Prefect", random.getLabel());

        File dir = doExport();
        assertEquals(4, dir.listFiles().length);
        File publicDir = new File(dir, "public");
        assertTrue(publicDir.exists() && publicDir.isDirectory());
        File arthurFile = new File(publicDir, root.getTopic().getId() + ".smsn");
        assertTrue(arthurFile.exists());
        List<String> lines = readLines(arthurFile);
        assertEquals(9, lines.size());
        assertEquals("@id " + root.getTopic().getId(), lines.get(0));
        assertEquals("@title Arthur Dent", lines.get(1));
        assertEquals("created", readPropertyLine(lines.get(2)).getKey());
        // note: no @weight or @priority
        assertEquals("@text ```", lines.get(3));
        assertEquals("He's a jerk.", lines.get(4));
        assertEquals("A complete kneebiter.", lines.get(5));
        assertEquals("```", lines.get(6));
        assertEquals("* :" + fordId + ": ", lines.get(7));
        assertEquals("* :" + zaphodId + ": ", lines.get(8));

    }

    private Map.Entry<String, String> readPropertyLine(final String line) {
        Map<String, String> map = new HashMap<>();
        int atIndex = line.indexOf('@');
        int spaceIndex = line.indexOf(' ');
        String key = line.substring(atIndex + 1, spaceIndex);
        String value = line.substring(spaceIndex + 1);
        map.put(key, value);
        return map.entrySet().iterator().next();
    }

    private List<String> readLines(final File file) throws IOException {
        List<String> lines = new LinkedList<>();
        try (InputStream in = new FileInputStream(file)) {
            BufferedReader br = new BufferedReader(new InputStreamReader(in));
            String line;
            while (null != (line = br.readLine())) {
                lines.add(line);
            }
        }
        return lines;
    }

    private File doExport() throws IOException {
        VCSWriter writer = new VCSWriter();

        File dir = createVCSTestDirectory();
        NoteWriter.Context context = new NoteWriter.Context();
        context.setDestDirectory(dir);
        context.setTopicGraph(topicGraph);

        writer.doWrite(context);
        return dir;
    }
}
