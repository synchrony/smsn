package net.fortytwo.smsn.server.io.graphml;

import net.fortytwo.smsn.brain.Brain;
import net.fortytwo.smsn.brain.model.pg.PGAtomGraph;
import net.fortytwo.smsn.server.io.BrainReader;
import net.fortytwo.smsn.server.io.Format;

import java.io.IOException;
import java.io.InputStream;
import java.util.Arrays;
import java.util.List;

/**
 * @author Joshua Shinavier (http://fortytwo.net)
 */
public class GraphMLReader extends BrainReader {

    @Override
    public List<Format> getFormats() {
        return Arrays.asList(GraphMLFormat.getInstance());
    }

    @Override
    protected void importInternal(Brain destBrain, final InputStream sourceStream, final Format format)
            throws IOException {
        if (destBrain.getAtomGraph() instanceof PGAtomGraph) {
            com.tinkerpop.blueprints.util.io.graphml.GraphMLReader r
                    = new com.tinkerpop.blueprints.util.io.graphml.GraphMLReader(
                    ((PGAtomGraph) destBrain.getAtomGraph()).getPropertyGraph());
            r.inputGraph(sourceStream);
        } else {
            throw new UnsupportedOperationException("GraphML I/O is not supported for this graph");
        }
    }
}
