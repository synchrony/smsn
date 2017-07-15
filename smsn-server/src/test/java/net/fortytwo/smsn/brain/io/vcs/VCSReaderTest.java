package net.fortytwo.smsn.brain.io.vcs;

import net.fortytwo.smsn.SemanticSynchrony;
import net.fortytwo.smsn.brain.BrainTestBase;
import net.fortytwo.smsn.brain.io.BrainReader;
import net.fortytwo.smsn.brain.io.Format;
import net.fortytwo.smsn.brain.model.TopicGraph;
import net.fortytwo.smsn.brain.model.entities.Atom;
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

        assertEquals(0, countAtoms());

        BrainReader.Context context = new BrainReader.Context();
        context.setFormat(formats.get(0));
        context.setSourceDirectory(createVCSTestDirectory());
        File universalDir = new File(SemanticSynchrony.getConfiguration().getSources().get(3).getLocation());
        copyVCSFileToDirectory(arthurTopic, universalDir);
        copyVCSFileToDirectory(fordTopic, universalDir);
        context.setTopicGraph(topicGraph);
        reader.doImport(context);

        assertEquals(2, countAtoms());

        Atom arthur = topicGraph.getAtomById(ARTHUR_ID).get();
        assertEquals(ARTHUR_ID, arthur.getId());
        assertEquals("Arthur Dent", arthur.getTitle());
        assertEquals(DefaultSources.UNIVERSAL, arthur.getSource());
        assertEquals(1344485170113L, (long) arthur.getCreated());
        assertEquals("He's a jerk.", arthur.getText());

        assertEquals(1, countChildren(arthur));
        Atom ford = arthur.getChildren().getFirst();
        assertEquals("Ford Prefect (character)", ford.getTitle());
        assertNull(ford.getText());
    }

    private void copyVCSFileToDirectory(final Topic topic, final File dir) throws IOException {
        String fileName = VCSFormat.fileNameForTopic(topic);
        try (InputStream in = VCSReaderTest.class.getResourceAsStream(fileName)) {
            try (OutputStream out = new FileOutputStream(new File(dir, fileName))) {
                IOUtils.copy(in, out);
            }
        }
    }
}
