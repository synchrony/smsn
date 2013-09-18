package net.fortytwo.extendo.brain.rdf.types;

import net.fortytwo.extendo.brain.rdf.BottomUpType;
import net.fortytwo.extendo.brain.rdf.Field;

import java.util.regex.Pattern;

/**
 * @author Joshua Shinavier (http://fortytwo.net)
 */
public class WebPage implements BottomUpType {
    public static final WebPage INSTANCE = new WebPage();

    private final Field[] fields = new Field[]{};

    private WebPage() {
    }

    public boolean additionalConstraintsSatisfied(final String value) {
        return true;
    }

    public Field[] getFields() {
        return fields;
    }

    public Pattern getValueRegex() {
        return Pattern.compile(".+ \\(web page\\)");
    }

    public boolean childrenRequired() {
        return false;
    }

    public boolean aliasRequired() {
        return true;
    }
}
