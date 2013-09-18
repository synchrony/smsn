package net.fortytwo.extendo.brain.rdf.types;

import net.fortytwo.extendo.brain.rdf.BottomUpType;
import net.fortytwo.extendo.brain.rdf.Field;

/**
 * @author Joshua Shinavier (http://fortytwo.net)
 */
public abstract class SimpleType implements BottomUpType {
    private final Field[] fields = new Field[] {};

    public Field[] getFields() {
        return fields;
    }

    public boolean childrenRequired() {
        return false;
    }

    public boolean aliasRequired() {
        return false;
    }
}
