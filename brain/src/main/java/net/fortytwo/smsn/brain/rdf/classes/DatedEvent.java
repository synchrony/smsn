package net.fortytwo.smsn.brain.rdf.classes;

import net.fortytwo.smsn.brain.model.entities.Note;
import net.fortytwo.smsn.brain.rdf.NoteClass;
import net.fortytwo.smsn.brain.rdf.NoteReqex;
import net.fortytwo.smsn.brain.rdf.RDFizationContext;
import org.eclipse.rdf4j.model.IRI;
import org.eclipse.rdf4j.model.Literal;
import org.eclipse.rdf4j.model.ValueFactory;
import org.eclipse.rdf4j.model.vocabulary.DCTERMS;
import org.eclipse.rdf4j.model.vocabulary.RDFS;
import org.eclipse.rdf4j.model.vocabulary.XMLSchema;
import org.eclipse.rdf4j.rio.RDFHandler;
import org.eclipse.rdf4j.rio.RDFHandlerException;

import java.util.Arrays;
import java.util.regex.Pattern;

public class DatedEvent extends NoteClass {

    public DatedEvent() {
        super(
                "dated-event",
                null,
                null,
                new NoteReqex(Arrays.asList(
                        // event starts with a date, followed by anything or nothing
                        new NoteReqex.El(new EventDateHandler(),
                                NoteReqex.Modifier.One, Date.class),
                        new NoteReqex.El(null,
                                NoteReqex.Modifier.ZeroOrMore)
                )));
    }

    @Override
    protected boolean isCollectionClass() {
        return false;
    }

    @Override
    public IRI toRDF(Note a, RDFizationContext context) throws RDFHandlerException {
        ValueFactory vf = context.getValueFactory();
        RDFHandler handler = context.getHandler();

        IRI self = handleTypeAndAlias(a, context, net.fortytwo.smsn.rdf.vocab.Event.Event);

        handler.handleStatement(vf.createStatement(self, RDFS.LABEL, vf.createLiteral(Note.getTitle(a))));

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
        public void handle(Note object, RDFizationContext context) throws RDFHandlerException {
            ValueFactory vf = context.getValueFactory();
            RDFHandler h = context.getHandler();

            String dateStr = Note.getTitle(object);

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
