package net.fortytwo.extendo.brain.rdf.types;

import java.util.regex.Pattern;

/**
 * @author Joshua Shinavier (http://fortytwo.net)
 */
public class Date extends SimpleSecondClassType {
    public static final Date INSTANCE = new Date();

    private Date() {
    }

    public boolean additionalConstraintsSatisfied(final String value) {
        return true;
    }

    public Pattern getValueRegex() {
        return Pattern.compile("[12][0-9][0-9][0-9]-[012][0-9]-[0123][0-9]");
    }
}
