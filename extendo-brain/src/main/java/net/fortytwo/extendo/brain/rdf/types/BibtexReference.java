package net.fortytwo.extendo.brain.rdf.types;

import java.util.regex.Pattern;

/**
 * @author Joshua Shinavier (http://fortytwo.net)
 */
public class BibtexReference extends SimpleType {
    public static final BibtexReference INSTANCE = new BibtexReference();

    private BibtexReference() {
    }

    public Pattern getValueRegex() {
        return Pattern.compile("bibtex: (.|\\s)+");
    }

    public boolean additionalConstraintsSatisfied(final String value) {
        return true;
    }
}
