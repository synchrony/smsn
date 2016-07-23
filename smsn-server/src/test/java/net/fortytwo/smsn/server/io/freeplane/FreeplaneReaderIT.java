package net.fortytwo.smsn.server.io.freeplane;

import com.tinkerpop.blueprints.Graph;
import com.tinkerpop.blueprints.KeyIndexableGraph;
import com.tinkerpop.blueprints.Vertex;
import com.tinkerpop.blueprints.impls.tg.TinkerGraph;
import net.fortytwo.smsn.brain.AtomGraph;
import net.fortytwo.smsn.brain.MyOtherBrain;
import net.fortytwo.smsn.server.io.BrainReader;
import net.fortytwo.smsn.server.io.BrainWriter;
import net.fortytwo.smsn.server.io.Format;
import net.fortytwo.smsn.server.io.graphml.GraphMLFormat;
import net.fortytwo.smsn.server.io.graphml.GraphMLWriter;
import org.junit.Test;

import java.io.File;
import java.io.FileOutputStream;
import java.io.OutputStream;

import static org.junit.Assert.assertNotNull;

/**
 * @author Joshua Shinavier (http://fortytwo.net)
 */
public class FreeplaneReaderIT {
    private File mindMapDirectory = new File("/Users/josh/projects/notmine/mindmaps");

    @Test
    public void testTmp() throws Exception {
        KeyIndexableGraph propertyGraph = new TinkerGraph();
        AtomGraph atomGraph = new AtomGraph(propertyGraph);
        MyOtherBrain brain = new MyOtherBrain(atomGraph);

        Format format = Format.getFormat("fReePLAne");
        assertNotNull(format);
        BrainReader reader = Format.getReader(format);
        assertNotNull(reader);

        reader.doImport(mindMapDirectory, FreeplaneFormat.getInstance(), brain, true);

        System.out.println("# vertices: " + countVertices(atomGraph));

        BrainWriter exporter = new GraphMLWriter();
        try (OutputStream out = new FileOutputStream(new File("/tmp/mindMap.xml"))) {
            exporter.doExport(brain, out, GraphMLFormat.getInstance());
        }
    }

    private int countVertices(final AtomGraph atomGraph) {
        Graph graph = atomGraph.getPropertyGraph();
        int count = 0;
        for (Vertex ignored : graph.getVertices()) {
            count++;
        }
        return count;
    }
}
