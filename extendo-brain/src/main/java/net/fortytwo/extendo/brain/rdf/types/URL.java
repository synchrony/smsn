package net.fortytwo.extendo.brain.rdf.types;

import java.util.regex.Pattern;

/**
 * @author Joshua Shinavier (http://fortytwo.net)
 */
public class URL extends SimpleType {
    public static final URL INSTANCE = new URL();

    private URL() {
    }

    public Pattern getValueRegex() {
        return Pattern.compile("http(s)?://.+");
    }

    public boolean fulfillsAdditionalConstraints(final String value) {
        return true;
    }
}
