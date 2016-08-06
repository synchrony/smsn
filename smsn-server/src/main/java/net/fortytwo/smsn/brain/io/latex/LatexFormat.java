package net.fortytwo.smsn.brain.io.latex;

import net.fortytwo.smsn.brain.io.Format;

/**
 * @author Joshua Shinavier (http://fortytwo.net)
 */
public class LatexFormat extends Format {
    private static final LatexFormat instance = new LatexFormat();

    private LatexFormat() {
        super("LaTeX", new String[]{"tex"});
    }

    public static LatexFormat getInstance() {
        return instance;
    }
}
