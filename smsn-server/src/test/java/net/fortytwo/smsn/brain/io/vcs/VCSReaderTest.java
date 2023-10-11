package net.fortytwo.smsn.brain.io.vcs;

import net.fortytwo.smsn.SemanticSynchrony;
import net.fortytwo.smsn.brain.BrainTestBase;
import net.fortytwo.smsn.brain.io.Format;
import net.fortytwo.smsn.brain.io.NoteReader;
import net.fortytwo.smsn.brain.model.TopicGraph;
import net.fortytwo.smsn.brain.model.entities.Note;
import net.fortytwo.smsn.brain.model.entities.Topic;
import org.apache.commons.io.IOUtils;
import org.junit.Test;

import java.io.File;
import java.io.FileOutputStream;
import java.io.IOException;
import java.io.InputStream;
import java.io.OutputStream;
import java.util.List;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertNull;

public class VCSReaderTest extends BrainTestBase {

    @Override
    protected TopicGraph createTopicGraph() throws IOException {
        return createNeo4jTopicGraph();
    }

    @Test
    public void formatIsCorrect() {
        List<Format> formats = new VCSReader().getFormats();
        assertEquals(1, formats.size());
        assertEquals("VCS", formats.get(0).getName());
    }

    @Test
    public void interdependentFilesAreReadCorrectly() throws Exception {
        VCSReader reader = new VCSReader();
        List<Format> formats = reader.getFormats();
        assertEquals(1, formats.size());
        assertEquals("VCS", formats.get(0).getName());

        assertEquals(0, countNotes());

        NoteReader.Context context = new NoteReader.Context();
        context.setFormat(formats.get(0));
        context.setSourceDirectory(createVCSTestDirectory());
        File universalDir = new File(SemanticSynchrony.getConfiguration().getSources().get(3).getLocation());
        copyVCSFileToDirectory(arthurTopic, universalDir);
        copyVCSFileToDirectory(fordTopic, universalDir);
        context.setTopicGraph(topicGraph);
        reader.doImport(context);

        assertEquals(2, countNotes());

        Note arthur = topicGraph.getNoteById(ARTHUR_ID).get();
        assertEquals(ARTHUR_ID, Note.getId(arthur));
        assertEquals("Arthur Dent", Note.getTitle(arthur));
        assertEquals(DefaultSources.UNIVERSAL, Note.getSource(arthur));
        assertEquals(1344485170113L, (long) Note.getCreated(arthur));
        assertEquals("He's a jerk.\nA real kneebiter.", Note.getText(arthur));

        assertEquals(1, countChildren(arthur));
        Note ford = arthur.getChildren().getFirst();
        assertEquals("Ford Prefect (character)", Note.getTitle(ford));
        assertNull(Note.getText(ford));
    }

    private void copyVCSFileToDirectory(final Topic topic, final File dir) throws IOException {
        String fileName = VCSFormat.fileNameForTopic(topic);
        try (InputStream in = VCSReaderTest.class.getResourceAsStream(fileName)) {
            if (in == null) {
                throw new IOException("failed to find resource: " + fileName);
            }
            try (OutputStream out = new FileOutputStream(new File(dir, fileName))) {
                IOUtils.copy(in, out);
            }
        }
    }
}
