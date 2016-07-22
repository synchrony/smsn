package net.fortytwo.smsn.server.io.graphml;

import net.fortytwo.smsn.brain.AtomGraph;
import net.fortytwo.smsn.brain.MyOtherBrain;
import net.fortytwo.smsn.server.io.BrainWriter;
import net.fortytwo.smsn.server.io.Format;

import java.io.IOException;
import java.io.OutputStream;
import java.util.Arrays;
import java.util.List;

/**
 * @author Joshua Shinavier (http://fortytwo.net)
 */
public class GraphMLWriter extends BrainWriter {

    @Override
    public List<Format> getFormats() {
        return Arrays.asList(GraphMLFormat.getInstance());
    }

    @Override
    protected void exportInternal(MyOtherBrain sourceBrain, OutputStream destStream, Format format)
            throws IOException {

        AtomGraph sourceGraph = sourceBrain.getAtomGraph();
        com.tinkerpop.blueprints.util.io.graphml.GraphMLWriter w = new com.tinkerpop.blueprints.util.io.graphml.GraphMLWriter(sourceGraph.getPropertyGraph());
        w.setNormalize(true);
        w.outputGraph(destStream);
    }
}
