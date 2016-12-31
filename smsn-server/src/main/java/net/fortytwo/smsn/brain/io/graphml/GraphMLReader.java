package net.fortytwo.smsn.brain.io.graphml;

import net.fortytwo.smsn.brain.io.BrainReader;
import net.fortytwo.smsn.brain.io.Format;
import net.fortytwo.smsn.brain.model.pg.PGAtomGraph;

import java.io.IOException;
import java.util.Arrays;
import java.util.List;

public class GraphMLReader extends BrainReader {

    @Override
    public List<Format> getFormats() {
        return Arrays.asList(GraphMLFormat.getInstance());
    }

    @Override
    protected void importInternal(Context context) throws IOException {

        if (context.getAtomGraph() instanceof PGAtomGraph) {
            org.apache.tinkerpop.gremlin.structure.io.graphml.GraphMLReader r
                    = org.apache.tinkerpop.gremlin.structure.io.graphml.GraphMLReader.build().create();
            r.readGraph(context.getSourceStream(), ((PGAtomGraph) context.getAtomGraph()).getPropertyGraph());
        } else {
            throw new UnsupportedOperationException("GraphML I/O is not supported for this graph");
        }
    }
}
