package net.fortytwo.smsn.brain.io.graphml;

import net.fortytwo.smsn.brain.io.NoteReader;
import net.fortytwo.smsn.brain.io.Format;
import net.fortytwo.smsn.brain.model.entities.Note;
import net.fortytwo.smsn.brain.model.TopicGraph;
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
            // note: no transaction buffering
            org.apache.tinkerpop.gremlin.structure.io.graphml.GraphMLReader r
                    = org.apache.tinkerpop.gremlin.structure.io.graphml.GraphMLReader.build().create();
            r.readGraph(context.getSourceStream(), ((PGTopicGraph) context.getTopicGraph()).getPropertyGraph());
        } else {
            throw new UnsupportedOperationException("GraphML I/O is not supported for this graph");
        }

        addAllToIndices(context.getTopicGraph());
    }

    private void addAllToIndices(TopicGraph destGraph) {
        for (Note a : destGraph.getAllNotes()) {
            String value = a.getTitle();
            if (null != value) destGraph.reindex(a);
        }
    }
}
