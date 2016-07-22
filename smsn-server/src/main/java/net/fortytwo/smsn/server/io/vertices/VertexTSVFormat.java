package net.fortytwo.smsn.server.io.vertices;

import net.fortytwo.smsn.server.io.Format;

/**
 * @author Joshua Shinavier (http://fortytwo.net)
 */
public class VertexTSVFormat extends Format {
    private static final VertexTSVFormat instance = new VertexTSVFormat();

    private VertexTSVFormat() {
        super("Vertices", new String[]{"tsv"});
    }

    public static VertexTSVFormat getInstance() {
        return instance;
    }
}
