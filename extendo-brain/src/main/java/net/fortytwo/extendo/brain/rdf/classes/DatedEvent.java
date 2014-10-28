package net.fortytwo.extendo.brain.rdf.classes;

import net.fortytwo.extendo.brain.Atom;
import net.fortytwo.extendo.brain.rdf.AtomClass;
import net.fortytwo.extendo.brain.rdf.AtomRegex;
import net.fortytwo.extendo.brain.rdf.RDFizationContext;
import net.fortytwo.extendo.rdf.vocab.Timeline;
import org.openrdf.model.Literal;
import org.openrdf.model.Resource;
import org.openrdf.model.URI;
import org.openrdf.model.ValueFactory;
import org.openrdf.model.vocabulary.RDF;
import org.openrdf.model.vocabulary.RDFS;
import org.openrdf.model.vocabulary.XMLSchema;
import org.openrdf.rio.RDFHandler;
import org.openrdf.rio.RDFHandlerException;

import java.util.Arrays;
import java.util.regex.Pattern;

/**
 * @author Joshua Shinavier (http://fortytwo.net)
 */
public class DatedEvent extends AtomClass {
    public static final DatedEvent INSTANCE = new DatedEvent();

    public DatedEvent() {
        super(
                "dated-event",
                null,
                null,
                new AtomRegex(Arrays.asList(
                        // event starts with a date, followed by anything or nothing
                        new AtomRegex.El(new EventDateHandler(),
                                AtomRegex.Modifier.One, Date.class),
                        new AtomRegex.El(null,
                                AtomRegex.Modifier.ZeroOrMore)
                )));
    }

    @Override
    protected boolean isCollectionClass() {
        return false;
    }

    @Override
    public URI toRDF(Atom a, RDFizationContext context) throws RDFHandlerException {
        ValueFactory vf = context.getValueFactory();
        RDFHandler handler = context.getHandler();

        URI self = handleTypeAndAlias(a, vf, handler, net.fortytwo.extendo.rdf.vocab.Event.Event);

        handler.handleStatement(vf.createStatement(self, RDFS.LABEL, vf.createLiteral(a.getValue())));

        return self;
    }

    public static class Birthday extends DatedEvent {
        public static final Birthday INSTANCE = new Birthday();

        public Birthday() {
            name = "birthday";
            valueRegex = Pattern.compile(".+ was born on .+");
        }
    }

    private static class EventDateHandler implements FieldHandler {
        @Override
        public void handle(Atom object, RDFizationContext context) throws RDFHandlerException {
            ValueFactory vf = context.getValueFactory();
            RDFHandler h = context.getHandler();

            String dateStr = object.getValue();

            Literal dateValue = vf.createLiteral(dateStr, XMLSchema.DATE);

            Resource interval = vf.createBNode();
            h.handleStatement(vf.createStatement(interval, RDF.TYPE, Timeline.Interval));
            h.handleStatement(vf.createStatement(interval, Timeline.at, dateValue));

            h.handleStatement(vf.createStatement(context.getSubjectUri(), net.fortytwo.extendo.rdf.vocab.Event.time, interval));
        }
    }
}
