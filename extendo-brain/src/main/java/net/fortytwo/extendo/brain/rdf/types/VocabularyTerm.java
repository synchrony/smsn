package net.fortytwo.extendo.brain.rdf.types;

import java.util.regex.Pattern;

/**
 * @author Joshua Shinavier (http://fortytwo.net)
 */
public class VocabularyTerm extends SimpleType {
    public static final VocabularyTerm INSTANCE = new VocabularyTerm();

    private VocabularyTerm() {
    }

    public Pattern getValueRegex() {
        return Pattern.compile("\\\".+\\\"");
    }

    public boolean fulfillsAdditionalConstraints(final String value) {
        return true;
    }
}
