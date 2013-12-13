package net.fortytwo.extendo.brain.rdf.types;


import org.apache.commons.validator.routines.ISBNValidator;

import java.util.regex.Pattern;

/**
 * @author Joshua Shinavier (http://fortytwo.net)
 */
public class ISBN extends SimpleSecondClassType {
    public static final ISBN INSTANCE = new ISBN();

    private static final ISBNValidator VALIDATOR = new ISBNValidator();

    private ISBN() {
        super("isbn");
    }

    public Pattern getValueRegex() {
        return Pattern.compile("ISBN: .+");
    }

    public boolean additionalConstraintsSatisfied(final String value) {
        // assuming exact "ISBN: " prefix
        String code = value.substring(6).trim();

        return VALIDATOR.isValid(code);
    }
}
