package net.fortytwo.smsn.brain.io.graphml;

import net.fortytwo.smsn.brain.Brain;
import net.fortytwo.smsn.brain.model.pg.PGAtomGraph;
import net.fortytwo.smsn.brain.io.BrainReader;
import net.fortytwo.smsn.brain.io.Format;

import java.io.IOException;
import java.io.InputStream;
import java.util.Arrays;
import java.util.List;

public class GraphMLReader extends BrainReader {

    @Override
    public List<Format> getFormats() {
        return Arrays.asList(GraphMLFormat.getInstance());
    }

    @Override
    protected void importInternal(Context context)
            throws IOException {
        if (context.getAtomGraph() instanceof PGAtomGraph) {
            com.tinkerpop.blueprints.util.io.graphml.GraphMLReader r
                    = new com.tinkerpop.blueprints.util.io.graphml.GraphMLReader(
                    ((PGAtomGraph) context.getAtomGraph()).getPropertyGraph());
            r.inputGraph(context.getSourceStream());
        } else {
            throw new UnsupportedOperationException("GraphML I/O is not supported for this graph");
        }
    }
}
