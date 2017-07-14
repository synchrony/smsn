package net.fortytwo.smsn.brain.io.graphml;

import net.fortytwo.smsn.brain.io.BrainWriter;
import net.fortytwo.smsn.brain.io.Format;
import net.fortytwo.smsn.brain.model.Filter;
import net.fortytwo.smsn.brain.model.pg.PGTopicGraph;

import java.io.IOException;
import java.util.Collections;
import java.util.List;

public class GraphMLWriter extends BrainWriter {

    @Override
    public List<Format> getFormats() {
        return Collections.singletonList(GraphMLFormat.getInstance());
    }

    @Override
    public void doExport(Context context) throws IOException {
        if (!(context.getTopicGraph() instanceof PGTopicGraph)) throw new UnsupportedOperationException();
        PGTopicGraph originalGraph = (PGTopicGraph) context.getTopicGraph();
        Filter filter = context.getFilter();
        PGTopicGraph newGraph = null == filter || filter.isTrivial()
                ? originalGraph
                : originalGraph.copyGraph(context.getFilter());

        org.apache.tinkerpop.gremlin.structure.io.graphml.GraphMLWriter writer
                = org.apache.tinkerpop.gremlin.structure.io.graphml.GraphMLWriter.build().normalize(true).create();
        writer.writeGraph(context.getDestStream(), newGraph.getPropertyGraph());
    }
}
