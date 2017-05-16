package net.fortytwo.smsn.brain.io.wiki;

import net.fortytwo.smsn.brain.io.Format;

import java.util.regex.Pattern;

public class WikiFormat extends Format {
    // regex of valid id suffixes
    static final Pattern ID_INFIX = Pattern.compile(":[a-zA-Z0-9]{5,}:");

    static final String VERBATIM_BLOCK_START = "{{{";
    static final String VERBATIM_BLOCK_END = "}}}";

    static final int MAX_BULLET_LENGTH = 1;

    // Tabs count as four spaces each.
    static final String TAB_REPLACEMENT = "    ";

    private static final WikiFormat instance = new WikiFormat();

    private WikiFormat() {
        super("Wiki", Type.Internal);
    }

    public static WikiFormat getInstance() {
        return instance;
    }
}
