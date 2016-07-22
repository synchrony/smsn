package net.fortytwo.smsn.server.io.graphml;

import net.fortytwo.smsn.server.io.Format;

/**
 * @author Joshua Shinavier (http://fortytwo.net)
 */
public class GraphMLFormat extends Format {
    private static final GraphMLFormat instance = new GraphMLFormat();

    private GraphMLFormat() {
        super("GraphML", new String[]{"graphml", "xml"});
    }

    public static GraphMLFormat getInstance() {
        return instance;
    }
}
