package net.fortytwo.smsn.server.io;

import com.tinkerpop.blueprints.util.io.graphml.GraphMLReader;
import net.fortytwo.smsn.brain.MyOtherBrain;

import java.io.IOException;
import java.io.InputStream;
import java.util.Arrays;
import java.util.List;

/**
 * @author Joshua Shinavier (http://fortytwo.net)
 */
public class GraphMLImporter extends Importer {
    public static final String FORMAT = "GraphML";

    @Override
    public List<String> getFormats() {
        return Arrays.asList(FORMAT);
    }

    @Override
    protected void importInternal(MyOtherBrain destBrain, final InputStream sourceStream) throws IOException {
        GraphMLReader r = new GraphMLReader(destBrain.getAtomGraph().getPropertyGraph());
        r.inputGraph(sourceStream);
    }
}
