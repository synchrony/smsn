package net.fortytwo.smsn.server.io;

import com.tinkerpop.blueprints.util.io.graphml.GraphMLWriter;
import net.fortytwo.smsn.brain.AtomGraph;
import net.fortytwo.smsn.brain.MyOtherBrain;

import java.io.IOException;
import java.io.OutputStream;
import java.util.Arrays;
import java.util.List;

/**
 * @author Joshua Shinavier (http://fortytwo.net)
 */
public class GraphMLExporter extends Exporter {
    public static final String FORMAT = "GraphML";

    @Override
    public List<String> getFormats() {
        return Arrays.asList(FORMAT);
    }

    @Override
    protected void exportInternal(MyOtherBrain sourceBrain, OutputStream destStream) throws IOException {
        AtomGraph sourceGraph = sourceBrain.getAtomGraph();
        GraphMLWriter w = new GraphMLWriter(sourceGraph.getPropertyGraph());
        w.setNormalize(true);
        w.outputGraph(destStream);
    }
}
