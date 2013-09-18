package net.fortytwo.extendo.brain.rdf.types;

import java.util.regex.Pattern;

/**
 * @author Joshua Shinavier (http://fortytwo.net)
 */
public class AKA extends SimpleType {
    public static final AKA INSTANCE = new AKA();

    private AKA() {
    }

    public Pattern getValueRegex() {
        return Pattern.compile("aka \\\".+\\\"");
    }

    public boolean additionalConstraintsSatisfied(final String value) {
        return true;
    }
}
