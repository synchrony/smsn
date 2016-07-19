package net.fortytwo.smsn.server.io;

import com.tinkerpop.blueprints.TransactionalGraph;
import com.tinkerpop.blueprints.util.io.graphml.GraphMLWriter;
import net.fortytwo.smsn.brain.BrainGraph;

import java.io.IOException;
import java.io.OutputStream;

/**
 * @author Joshua Shinavier (http://fortytwo.net)
 */
public class GraphMLExporter extends Exporter {
    @Override
    protected void exportInternal(BrainGraph sourceGraph, OutputStream destStream) throws IOException {
        //((TransactionalGraph) g.getPropertyGraph()).commit();

        GraphMLWriter w = new GraphMLWriter(sourceGraph.getPropertyGraph());
        w.setNormalize(true);
        w.outputGraph(destStream);
    }
}
