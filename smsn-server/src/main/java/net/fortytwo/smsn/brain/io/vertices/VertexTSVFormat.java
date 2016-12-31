package net.fortytwo.smsn.brain.io.vertices;

import net.fortytwo.smsn.brain.io.Format;

public class VertexTSVFormat extends Format {
    private static final VertexTSVFormat instance = new VertexTSVFormat();

    private VertexTSVFormat() {
        super("Vertices", Type.FileBased, "tsv");
    }

    public static VertexTSVFormat getInstance() {
        return instance;
    }
}
