package net.fortytwo.smsn.brain.io.graphml;

import net.fortytwo.smsn.brain.io.NoteReader;
import net.fortytwo.smsn.brain.io.Format;
import net.fortytwo.smsn.brain.model.pg.PGTopicGraph;

import java.io.IOException;
import java.util.Collections;
import java.util.List;

public class GraphMLReader extends NoteReader {

    @Override
    public List<Format> getFormats() {
        return Collections.singletonList(GraphMLFormat.getInstance());
    }

    @Override
    protected void importInternal(Context context) throws IOException {

        if (context.getTopicGraph() instanceof PGTopicGraph) {
            PGTopicGraph pgGraph = (PGTopicGraph) context.getTopicGraph();
            // note: no transaction buffering
            org.apache.tinkerpop.gremlin.structure.io.graphml.GraphMLReader reader
                    = org.apache.tinkerpop.gremlin.structure.io.graphml.GraphMLReader.build().create();
            reader.readGraph(context.getSourceStream(), pgGraph.getPropertyGraph());

            // Note: GraphML import writes directly to the property graph.
            // Reindexing would require iterating over all imported atoms,
            // but this is deferred to the caller if needed.
        } else {
            throw new UnsupportedOperationException("GraphML I/O is not supported for this graph");
        }
    }
}
