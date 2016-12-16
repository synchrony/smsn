package net.fortytwo.smsn.brain.io.latex;

import net.fortytwo.smsn.brain.io.Format;

public class LatexFormat extends Format {
    private static final LatexFormat instance = new LatexFormat();

    private LatexFormat() {
        super("LaTeX", Type.FileBased, "tex");
    }

    public static LatexFormat getInstance() {
        return instance;
    }
}
