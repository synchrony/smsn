package net.fortytwo.smsn.brain.io.graphml;

import net.fortytwo.smsn.brain.io.BrainWriter;
import net.fortytwo.smsn.brain.io.Format;
import net.fortytwo.smsn.brain.model.Filter;
import net.fortytwo.smsn.brain.model.pg.PGAtomGraph;

import java.io.IOException;
import java.util.Arrays;
import java.util.List;

public class GraphMLWriter extends BrainWriter {

    @Override
    public List<Format> getFormats() {
        return Arrays.asList(GraphMLFormat.getInstance());
    }

    @Override
    public void doExport(Context context) throws IOException {
        if (!(context.getAtomGraph() instanceof PGAtomGraph)) throw new UnsupportedOperationException();
        PGAtomGraph originalGraph = (PGAtomGraph) context.getAtomGraph();
        Filter filter = context.getFilter();
        PGAtomGraph newGraph = null == filter || filter.isTrivial()
                ? originalGraph
                : originalGraph.copyGraph(context.getFilter());

        org.apache.tinkerpop.gremlin.structure.io.graphml.GraphMLWriter writer
                = org.apache.tinkerpop.gremlin.structure.io.graphml.GraphMLWriter.build().normalize(true).create();
        writer.writeGraph(context.getDestStream(), newGraph.getPropertyGraph());
    }
}
