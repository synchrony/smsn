package net.fortytwo.smsn.brain.io.pages;

import net.fortytwo.smsn.brain.io.Format;

import java.util.regex.Pattern;

public class PageFormat extends Format {
    static final String TEXT_DELIMITER = "```";
    static final String NOUN_BULLET = "*";
    static final String VERB_BULLET = "--";

    // regex of valid id suffixes
    static final Pattern ID_INFIX = Pattern.compile(":[a-zA-Z0-9]{5,}:");

    // Tabs count as four spaces each.
    static final String TAB_REPLACEMENT = "    ";

    private static final PageFormat instance = new PageFormat();

    private PageFormat() {
        super("Page", Type.Internal);
    }

    public static PageFormat getInstance() {
        return instance;
    }
}
