package net.fortytwo.smsn.brain.io.graphml;

import com.tinkerpop.blueprints.Graph;
import com.tinkerpop.blueprints.Vertex;
import com.tinkerpop.blueprints.impls.tg.TinkerGraph;
import net.fortytwo.smsn.brain.io.BrainWriter;
import net.fortytwo.smsn.brain.io.Format;
import net.fortytwo.smsn.brain.model.Atom;
import net.fortytwo.smsn.brain.model.AtomGraph;
import net.fortytwo.smsn.brain.model.AtomList;
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

        com.tinkerpop.blueprints.util.io.graphml.GraphMLWriter w
                = new com.tinkerpop.blueprints.util.io.graphml.GraphMLWriter(
                newGraph.getPropertyGraph());
        w.setNormalize(true);
        w.outputGraph(context.getDestStream());
    }
}
