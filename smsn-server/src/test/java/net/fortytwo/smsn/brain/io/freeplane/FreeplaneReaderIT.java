package net.fortytwo.smsn.brain.io.freeplane;

import com.tinkerpop.blueprints.KeyIndexableGraph;
import com.tinkerpop.blueprints.impls.tg.TinkerGraph;
import net.fortytwo.smsn.brain.Brain;
import net.fortytwo.smsn.brain.model.Atom;
import net.fortytwo.smsn.brain.model.AtomGraph;
import net.fortytwo.smsn.brain.model.pg.PGAtomGraph;
import net.fortytwo.smsn.brain.io.BrainReader;
import net.fortytwo.smsn.brain.io.BrainWriter;
import net.fortytwo.smsn.brain.io.Format;
import net.fortytwo.smsn.brain.io.graphml.GraphMLFormat;
import net.fortytwo.smsn.brain.io.graphml.GraphMLWriter;
import org.junit.Test;

import java.io.File;
import java.io.FileOutputStream;
import java.io.OutputStream;

import static org.junit.Assert.assertNotNull;

public class FreeplaneReaderIT {
    private final File mindMapDirectory = new File("/Users/josh/projects/notmine/mindmaps");

    @Test
    public void testTmp() throws Exception {
        KeyIndexableGraph propertyGraph = new TinkerGraph();
        AtomGraph atomGraph = new PGAtomGraph(propertyGraph);
        Brain brain = new Brain(atomGraph);

        Format format = Format.getFormat("fReePLAne");
        assertNotNull(format);
        BrainReader reader = Format.getReader(format);
        assertNotNull(reader);

        reader.doImport(mindMapDirectory, FreeplaneFormat.getInstance(), brain, true);

        System.out.println("# atoms: " + countAtoms(atomGraph));

        BrainWriter exporter = new GraphMLWriter();
        try (OutputStream out = new FileOutputStream(new File("/tmp/mindMap.xml"))) {
            BrainWriter.Context context = new BrainWriter.Context();
            context.setAtomGraph(brain.getAtomGraph());
            context.setKnowledgeBase(brain.getKnowledgeBase());
            context.setDestStream(out);
            context.setFormat(GraphMLFormat.getInstance());
            exporter.doExport(context);
        }
    }

    private int countAtoms(final AtomGraph atomGraph) {
        int count = 0;
        for (Atom ignored : atomGraph.getAllAtoms()) {
            count++;
        }
        return count;
    }
}
