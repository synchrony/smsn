package net.fortytwo.smsn.brain.rdf.classes;

import net.fortytwo.smsn.brain.model.Atom;
import net.fortytwo.smsn.brain.rdf.AtomClass;
import net.fortytwo.smsn.brain.rdf.AtomRegex;
import net.fortytwo.smsn.brain.rdf.RDFizationContext;
import org.openrdf.model.IRI;
import org.openrdf.model.Literal;
import org.openrdf.model.ValueFactory;
import org.openrdf.model.vocabulary.DCTERMS;
import org.openrdf.model.vocabulary.RDFS;
import org.openrdf.model.vocabulary.XMLSchema;
import org.openrdf.rio.RDFHandler;
import org.openrdf.rio.RDFHandlerException;

import java.util.Arrays;
import java.util.regex.Pattern;

public class DatedEvent extends AtomClass {

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
    public IRI toRDF(Atom a, RDFizationContext context) throws RDFHandlerException {
        ValueFactory vf = context.getValueFactory();
        RDFHandler handler = context.getHandler();

        IRI self = handleTypeAndAlias(a, context, net.fortytwo.smsn.rdf.vocab.Event.Event);

        handler.handleStatement(vf.createStatement(self, RDFS.LABEL, vf.createLiteral(a.getTitle())));

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

            String dateStr = object.getTitle();

            Literal dateValue = vf.createLiteral(dateStr, XMLSchema.DATE);

            h.handleStatement(vf.createStatement(context.getSubjectIri(), DCTERMS.DATE, dateValue));

            /* Alternative, using the Event Ontology (DO NOT DELETE):

            Resource interval = vf.createBNode();
            h.handleStatement(vf.createStatement(interval, RDF.TYPE, Timeline.Interval));
            h.handleStatement(vf.createStatement(interval, Timeline.at, dateValue));

            h.handleStatement(vf.createStatement(
                    context.getSubjectIri(), net.fortytwo.smsn.rdf.vocab.Event.time, interval));
            */

            // note: a third possibility is to give Dates individual IRI
        }
    }
}
