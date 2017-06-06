package net.fortytwo.smsn.brain.io.vcs;

import net.fortytwo.smsn.SemanticSynchrony;
import net.fortytwo.smsn.brain.BrainTestBase;
import net.fortytwo.smsn.brain.io.BrainReader;
import net.fortytwo.smsn.brain.io.Format;
import net.fortytwo.smsn.brain.model.TopicGraph;
import net.fortytwo.smsn.brain.model.entities.Atom;
import net.fortytwo.smsn.brain.model.entities.EntityList;
import net.fortytwo.smsn.config.DataSource;
import org.apache.commons.io.IOUtils;
import org.junit.Test;

import java.io.File;
import java.io.FileOutputStream;
import java.io.IOException;
import java.io.InputStream;
import java.io.OutputStream;
import java.util.List;

import static org.junit.Assert.assertEquals;

public class VCSReaderTest extends BrainTestBase {

    @Override
    protected TopicGraph createAtomGraph() throws IOException {
        return createNeo4jAtomGraph();
    }

    @Test
    public void interconnectedFilesAreReadCorrectly() throws IOException {
        VCSReader reader = new VCSReader();
        List<Format> formats = reader.getFormats();
        assertEquals(1, formats.size());
        assertEquals("VCS", formats.get(0).getName());

        assertEquals(0, countAtoms());

        BrainReader.Context context = new BrainReader.Context();
        context.setFormat(formats.get(0));
        context.setSourceDirectory(createTestDirectory());
        context.setTopicGraph(topicGraph);
        reader.doImport(context);

        assertEquals(2, countAtoms());

        Atom arthur = topicGraph.getAtomById(ARTHUR_ID).get();
        assertEquals(ARTHUR_ID, arthur.getId());
        assertEquals("Arthur Dent", arthur.getTitle());
        assertEquals(DefaultSources.UNIVERSAL, arthur.getSource());

        assertEquals(1, countChildren(arthur));
        assertEquals("Ford Prefect (character)", arthur.getChildren().getFirst().getTitle());
    }

    private long countChildren(final Atom a) {
        EntityList<Atom> children = a.getChildren();
        return null == children ? 0 : EntityList.toJavaList(children).size();
    }

    private File createTestDirectory() throws IOException {
        File dir = createTempDirectory();

        File privateDir = new File(dir, "private"); privateDir.mkdir();
        File personalDir = new File(dir, "personal"); personalDir.mkdir();
        File publicDir = new File(dir, "public"); publicDir.mkdir();
        File universalDir = new File(dir, "universal"); universalDir.mkdir();

        List<DataSource> sources = SemanticSynchrony.getConfiguration().getSources();
        sources.get(0).setLocation(privateDir.getAbsolutePath());
        sources.get(1).setLocation(personalDir.getAbsolutePath());
        sources.get(2).setLocation(publicDir.getAbsolutePath());
        sources.get(3).setLocation(universalDir.getAbsolutePath());

        copyVCSFileToDirectory(ARTHUR_ID, universalDir);
        copyVCSFileToDirectory(FORD_ID, universalDir);

        return dir;
    }

    private void copyVCSFileToDirectory(final String atomId, final File dir) throws IOException {
        try (InputStream in = VCSReaderTest.class.getResourceAsStream(atomId)) {
            try (OutputStream out = new FileOutputStream(new File(dir, atomId))) {
                IOUtils.copy(in, out);
            }
        }
    }
}
