package net.fortytwo.extendo.brain.rdf.types;

import net.fortytwo.extendo.brain.rdf.BottomUpType;
import net.fortytwo.extendo.brain.rdf.Field;

/**
 * @author Joshua Shinavier (http://fortytwo.net)
 */
public abstract class SimpleType extends BottomUpType {
    private Field[] fields = null;

    protected SimpleType(String name) {
        super(name);
    }

    public Field[] getFields() {
        if (null == fields) {
            fields = new Field[] {};
        }

        return fields;
    }

    public boolean childrenRequired() {
        return false;
    }

    public boolean aliasRequired() {
        return false;
    }
}
