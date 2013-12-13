package net.fortytwo.extendo.brain.rdf.types;

import java.util.regex.Pattern;

/**
 * @author Joshua Shinavier (http://fortytwo.net)
 */
public class AKA extends SimpleSecondClassType {
    public static final AKA INSTANCE = new AKA();

    private AKA() {
        super("aka");
    }

    public Pattern getValueRegex() {
        return Pattern.compile("aka \\\"[^\\\"]+\\\"(, \\\"[^\\\"]+\\\")*");
        //return Pattern.compile("aka \\\"[^\\\"]+\\\"");
    }

    public boolean additionalConstraintsSatisfied(final String value) {
        return true;
    }

    public static String extractAlias(final String value) {
        int i = value.indexOf('\"');
        int j = value.lastIndexOf('\"');
        return value.substring(i + 1, j);
    }
}
