package net.fortytwo.extendo.brain.rdf.types;

import net.fortytwo.extendo.brain.Atom;
import net.fortytwo.extendo.brain.rdf.BottomUpType;
import net.fortytwo.extendo.brain.rdf.Field;
import net.fortytwo.extendo.brain.rdf.Mapper;

import java.util.regex.Pattern;

/**
 * @author Joshua Shinavier (http://fortytwo.net)
 */
public class TimeStampedEvent implements BottomUpType {
    public static final TimeStampedEvent INSTANCE = new TimeStampedEvent();

    private Field[] fields = new Field[]{
            new Field(true, null, Date.INSTANCE, null, new EventDateMapper())
    };

    private TimeStampedEvent() {
    }

    public Field[] getFields() {
        return fields;
    }

    public Pattern getValueRegex() {
        return null;
    }

    public boolean additionalConstraintsSatisfied(final String value) {
        return true;
    }

    public boolean childrenRequired() {
        return true;
    }

    public boolean aliasRequired() {
        return false;
    }

    private class EventDateMapper implements Mapper {
        @Override
        public void mapToRDF(Atom parent, Atom child) {
            //To change body of implemented methods use File | Settings | File Templates.
        }
    }
}
