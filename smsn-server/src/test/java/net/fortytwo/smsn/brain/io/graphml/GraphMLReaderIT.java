package net.fortytwo.smsn.brain.io.graphml;

import net.fortytwo.smsn.brain.Brain;
import net.fortytwo.smsn.brain.BrainTestBase;
import net.fortytwo.smsn.brain.io.BrainReader;
import net.fortytwo.smsn.brain.io.BrainWriter;
import net.fortytwo.smsn.brain.io.Format;
import net.fortytwo.smsn.brain.model.AtomGraph;
import org.junit.Test;

import java.io.File;
import java.io.FileOutputStream;
import java.io.IOException;
import java.io.OutputStream;

import static org.junit.Assert.assertNotNull;

public class GraphMLReaderIT extends BrainTestBase {

    private final File inputFile = new File("/tmp/smsn.xml");
    private final File outputFile = new File("/tmp/smsn-out.xml");

    @Override
    protected AtomGraph createAtomGraph() throws IOException {
        return createNeo4jAtomGraph();
    }

    @Test
    public void testTmp() throws Exception {
        Brain brain = new Brain(atomGraph);

        Format format = Format.getFormat("graphml");
        assertNotNull(format);
        BrainReader reader = Format.getReader(format);
        assertNotNull(reader);

        reader.doImport(inputFile, GraphMLFormat.getInstance(), brain, true);

        System.out.println("# atoms: " + countAtoms(atomGraph));

        BrainWriter exporter = new GraphMLWriter();
        try (OutputStream out = new FileOutputStream(outputFile)) {
            BrainWriter.Context context = new BrainWriter.Context();
            context.setAtomGraph(brain.getAtomGraph());
            context.setKnowledgeBase(brain.getKnowledgeBase());
            context.setDestStream(out);
            context.setFormat(GraphMLFormat.getInstance());
            exporter.doExport(context);
        }
    }
}
