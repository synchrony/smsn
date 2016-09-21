package net.fortytwo.smsn.brain.io.graphml;

import net.fortytwo.smsn.brain.io.Format;
import org.apache.tinkerpop.gremlin.structure.io.graphml.*;

public class GraphMLFormat extends Format {
    private static final GraphMLFormat instance = new GraphMLFormat();

    private GraphMLFormat() {
        super("GraphML", new String[]{"graphml", "xml"});
    }

    public static GraphMLFormat getInstance() {
        return instance;
    }
}
