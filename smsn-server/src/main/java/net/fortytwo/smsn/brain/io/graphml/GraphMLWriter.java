package net.fortytwo.smsn.brain.io.graphml;

import net.fortytwo.smsn.brain.model.AtomGraph;
import net.fortytwo.smsn.brain.Brain;
import net.fortytwo.smsn.brain.model.pg.PGAtomGraph;
import net.fortytwo.smsn.brain.io.BrainWriter;
import net.fortytwo.smsn.brain.io.Format;

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
    protected void exportInternal(Brain sourceBrain, OutputStream destStream, Format format)
            throws IOException {

        if (sourceBrain.getAtomGraph() instanceof PGAtomGraph) {
            AtomGraph sourceGraph = sourceBrain.getAtomGraph();
            com.tinkerpop.blueprints.util.io.graphml.GraphMLWriter w
                    = new com.tinkerpop.blueprints.util.io.graphml.GraphMLWriter(
                    ((PGAtomGraph) sourceGraph).getPropertyGraph());
            w.setNormalize(true);
            w.outputGraph(destStream);
        } else {
            throw new UnsupportedOperationException("GraphML I/O is not supported for this graph");
        }
    }
}
