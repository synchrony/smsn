package net.fortytwo.smsn.brain.io.wiki;

import net.fortytwo.smsn.brain.io.Format;

import java.util.regex.Pattern;

public class WikiFormat extends Format {

    // A special value, for incoming notes only,
    // which causes an atom's alias or shortcut to be set to null (rather than merely ignored)
    public static final String CLEARME = "C.L.E.A.R.M.E";

    static final String NOUN_BULLET = "*";
    static final String VERB_BULLET = "--";

    // regex of valid id suffixes
    static final Pattern ID_INFIX = Pattern.compile(":[a-zA-Z0-9]{5,}:");

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
