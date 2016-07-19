package net.fortytwo.smsn.server.io;

import com.tinkerpop.blueprints.util.io.graphml.GraphMLReader;
import net.fortytwo.smsn.brain.BrainGraph;

import java.io.IOException;
import java.io.InputStream;

/**
 * @author Joshua Shinavier (http://fortytwo.net)
 */
public class GraphMLImporter extends Importer {
    @Override
    protected void importInternal(BrainGraph destGraph, final InputStream sourceStream) throws IOException {
        GraphMLReader r = new GraphMLReader(destGraph.getPropertyGraph());
        r.inputGraph(sourceStream);
    }
}
