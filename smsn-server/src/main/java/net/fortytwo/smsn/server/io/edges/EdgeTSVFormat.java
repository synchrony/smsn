package net.fortytwo.smsn.server.io.edges;

import net.fortytwo.smsn.server.io.Format;

/**
 * @author Joshua Shinavier (http://fortytwo.net)
 */
public class EdgeTSVFormat extends Format {
    private static final EdgeTSVFormat instance = new EdgeTSVFormat();

    private EdgeTSVFormat() {
        super("Edge", new String[]{"tsv"});
    }

    public static EdgeTSVFormat getInstance() {
        return instance;
    }
}
