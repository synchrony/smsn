package net.fortytwo.smsn.brain.io.graphml;

import net.fortytwo.smsn.brain.io.BrainReader;
import net.fortytwo.smsn.brain.io.Format;
import net.fortytwo.smsn.brain.model.Atom;
import net.fortytwo.smsn.brain.model.AtomGraph;
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
            // note: no transaction buffering
            org.apache.tinkerpop.gremlin.structure.io.graphml.GraphMLReader r
                    = org.apache.tinkerpop.gremlin.structure.io.graphml.GraphMLReader.build().create();
            r.readGraph(context.getSourceStream(), ((PGAtomGraph) context.getAtomGraph()).getPropertyGraph());
        } else {
            throw new UnsupportedOperationException("GraphML I/O is not supported for this graph");
        }

        addAllToIndices(context.getAtomGraph());
    }

    private void addAllToIndices(AtomGraph destGraph) {
        for (Atom a : destGraph.getAllAtoms()) {
            String value = a.getValue();
            if (null != value) destGraph.reindexAtom(a);
        }
    }
}
