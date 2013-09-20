package net.fortytwo.extendo.brain.rdf.types;

import net.fortytwo.extendo.brain.Atom;
import net.fortytwo.extendo.brain.rdf.BottomUpType;
import net.fortytwo.extendo.brain.rdf.Field;
import net.fortytwo.extendo.brain.rdf.Mapper;
import net.fortytwo.extendo.brain.rdf.vocab.Event;
import org.openrdf.model.URI;
import org.openrdf.model.ValueFactory;
import org.openrdf.model.vocabulary.RDFS;
import org.openrdf.rio.RDFHandler;
import org.openrdf.rio.RDFHandlerException;

import java.util.regex.Pattern;

/**
 * @author Joshua Shinavier (http://fortytwo.net)
 */
public class TimeStampedEvent extends BottomUpType {
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

    public void translateToRDF(final Atom a,
                               final ValueFactory vf,
                               final RDFHandler handler) throws RDFHandlerException {
        URI self = translateTypeAndAlias(a, vf, handler, Event.Event);

        handler.handleStatement(vf.createStatement(self, RDFS.LABEL, vf.createLiteral(a.getValue())));
    }

    private class EventDateMapper implements Mapper {
        public void mapToRDF(Atom parent, Atom child) {
            //To change body of implemented methods use File | Settings | File Templates.
        }
    }
}
