package net.fortytwo.extendo.brain.rdf.types;

import net.fortytwo.extendo.Extendo;
import net.fortytwo.extendo.brain.Atom;
import net.fortytwo.extendo.brain.rdf.BottomUpType;
import net.fortytwo.extendo.brain.rdf.Field;
import net.fortytwo.extendo.brain.rdf.Mapper;
import net.fortytwo.extendo.brain.rdf.vocab.Event;
import net.fortytwo.extendo.brain.rdf.vocab.Timeline;
import org.openrdf.model.Literal;
import org.openrdf.model.Resource;
import org.openrdf.model.URI;
import org.openrdf.model.ValueFactory;
import org.openrdf.model.vocabulary.RDF;
import org.openrdf.model.vocabulary.RDFS;
import org.openrdf.model.vocabulary.XMLSchema;
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

    public URI translateToRDF(final Atom a,
                               final ValueFactory vf,
                               final RDFHandler handler) throws RDFHandlerException {
        URI self = translateTypeAndAlias(a, vf, handler, Event.Event);

        handler.handleStatement(vf.createStatement(self, RDFS.LABEL, vf.createLiteral(a.getValue())));

        return self;
    }

    private class EventDateMapper implements Mapper {
        public void mapToRDF(final Atom parent,
                             final Atom eventDate,
                             final URI parentURI,
                             final ValueFactory vf,
                             final RDFHandler handler) throws RDFHandlerException {

            String dateStr = eventDate.getValue();

            // this is a precaution against bugs which creep into the code; it should never happen
            if (Extendo.SAFE_MODE && !Date.INSTANCE.getValueRegex().matcher(dateStr).matches()) {
                throw new IllegalStateException("timestamp of event expected to be a date: " + dateStr);
            }

            Literal dateValue = vf.createLiteral(dateStr, XMLSchema.DATE);

            Resource interval = vf.createBNode();
            handler.handleStatement(vf.createStatement(interval, RDF.TYPE, Timeline.Interval));
            handler.handleStatement(vf.createStatement(interval, Timeline.at, dateValue));

            handler.handleStatement(vf.createStatement(parentURI, Event.time, interval));
        }
    }
}
