package net.fortytwo.extendo.brain.rdf.types;

import java.util.regex.Pattern;

/**
 * @author Joshua Shinavier (http://fortytwo.net)
 */
public class TODO extends SimpleType {
    public static final TODO INSTANCE = new TODO();

    private TODO() {
    }

    public boolean additionalConstraintsSatisfied(final String value) {
        return true;
    }

    public Pattern getValueRegex() {
        return Pattern.compile("TODO: .+");
    }
}
