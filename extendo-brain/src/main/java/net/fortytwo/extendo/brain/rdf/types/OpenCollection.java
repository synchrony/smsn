package net.fortytwo.extendo.brain.rdf.types;

import net.fortytwo.extendo.brain.rdf.BottomUpType;
import net.fortytwo.extendo.brain.rdf.Field;

import java.util.regex.Pattern;

/**
 * @author Joshua Shinavier (http://fortytwo.net)
 */
public class OpenCollection implements BottomUpType {
    public static final OpenCollection INSTANCE = new OpenCollection();

    private final Field[] fields = new Field[] {};

    public Field[] getFields() {
        return fields;
    }

    public Pattern getValueRegex() {
        return Pattern.compile("some .+");
    }

    public boolean childrenRequired() {
        return true;
    }

    public boolean aliasRequired() {
        return false;
    }
}
