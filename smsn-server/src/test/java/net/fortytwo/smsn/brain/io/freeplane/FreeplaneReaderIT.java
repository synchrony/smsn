package net.fortytwo.smsn.brain.io.freeplane;

import net.fortytwo.smsn.brain.Brain;
import net.fortytwo.smsn.brain.BrainTestBase;
import net.fortytwo.smsn.brain.io.BrainReader;
import net.fortytwo.smsn.brain.io.BrainWriter;
import net.fortytwo.smsn.brain.io.Format;
import net.fortytwo.smsn.brain.io.graphml.GraphMLFormat;
import net.fortytwo.smsn.brain.io.graphml.GraphMLWriter;
import net.fortytwo.smsn.brain.model.TopicGraph;
import org.junit.Test;

import java.io.File;
import java.io.FileOutputStream;
import java.io.IOException;
import java.io.OutputStream;

import static org.junit.Assert.assertNotNull;

public class FreeplaneReaderIT extends BrainTestBase {
    private final File mindMapDirectory = new File("/Users/josh/projects/notmine/mindmaps");

    @Override
    protected TopicGraph createAtomGraph() throws IOException {
        return createTinkerAtomGraph();
    }

    @Test
    public void testTmp() throws Exception {
        Brain brain = new Brain(topicGraph);

        Format format = Format.getFormat("fReePLAne");
        assertNotNull(format);
        BrainReader reader = Format.getReader(format);
        assertNotNull(reader);

        reader.doImport(mindMapDirectory, FreeplaneFormat.getInstance(), brain, true);

        System.out.println("# atoms: " + countAtoms());

        BrainWriter exporter = new GraphMLWriter();
        try (OutputStream out = new FileOutputStream(new File("/tmp/mindMap.xml"))) {
            BrainWriter.Context context = new BrainWriter.Context();
            context.setTopicGraph(brain.getTopicGraph());
            context.setKnowledgeBase(brain.getKnowledgeBase());
            context.setDestStream(out);
            context.setFormat(GraphMLFormat.getInstance());
            exporter.doExport(context);
        }
    }
}
