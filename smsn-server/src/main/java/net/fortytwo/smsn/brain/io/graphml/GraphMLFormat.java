package net.fortytwo.smsn.brain.io.graphml;

import net.fortytwo.smsn.brain.io.Format;

public class GraphMLFormat extends Format {
    private static final GraphMLFormat instance = new GraphMLFormat();

    private GraphMLFormat() {
        super("GraphML", Type.FileBased, "graphml", "xml");
    }

    public static GraphMLFormat getInstance() {
        return instance;
    }
}
