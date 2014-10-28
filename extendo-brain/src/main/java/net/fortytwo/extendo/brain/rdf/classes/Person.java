package net.fortytwo.extendo.brain.rdf.classes;

import net.fortytwo.extendo.brain.Atom;
import net.fortytwo.extendo.brain.rdf.AtomClass;
import net.fortytwo.extendo.brain.rdf.AtomRegex;
import net.fortytwo.extendo.brain.rdf.RDFizationContext;
import net.fortytwo.extendo.brain.rdf.classes.collections.InterestCollection;
import net.fortytwo.extendo.brain.rdf.classes.collections.DocumentCollection;
import net.fortytwo.extendo.brain.rdf.classes.collections.PersonCollection;
import net.fortytwo.extendo.brain.rdf.classes.collections.QuotedValueCollection;
import net.fortytwo.extendo.rdf.vocab.FOAF;
import org.openrdf.model.URI;
import org.openrdf.model.ValueFactory;
import org.openrdf.model.vocabulary.RDFS;
import org.openrdf.rio.RDFHandler;
import org.openrdf.rio.RDFHandlerException;

import java.util.Arrays;
import java.util.regex.Pattern;

/**
 * @author Joshua Shinavier (http://fortytwo.net)
 */
public class Person extends AtomClass {

    public Person() {
        super(
                "person",
                // note: currently excludes names which begin with special characters (e.g. Chinese or certain European names)
                Pattern.compile("[A-Z].+"),
                null,
                new AtomRegex(Arrays.asList(
                        new AtomRegex.El(new NickHandler(),
                                AtomRegex.Modifier.ZeroOrOne, AKAReference.class),
                        new AtomRegex.El(new HomepageHandler(),
                                AtomRegex.Modifier.ZeroOrMore, WebPage.class),
                        new AtomRegex.El(new MadeHandler(),
                                AtomRegex.Modifier.ZeroOrOne, DocumentCollection.class),
                        new AtomRegex.El(new QuotationHandler(),
                                AtomRegex.Modifier.ZeroOrOne, QuotedValueCollection.class),
                        new AtomRegex.El(new InterestHandler(),
                                AtomRegex.Modifier.ZeroOrOne, InterestCollection.class),
                        new AtomRegex.El(new KnowsHandler(),
                                AtomRegex.Modifier.ZeroOrMore, PersonCollection.class),
                        new AtomRegex.El(new BirthdayHandler(),
                                AtomRegex.Modifier.ZeroOrOne, DatedEvent.Birthday.class),
                        // TODO: when the person passed away
                        // TODO: the person's contact information
                        // TODO: the person's email
                        // TODO: the person's mailing address
                        // TODO: some things liked about the person
                        // TODO: some things learned about from the person
                        // TODO: memories of the person
                        // TODO: relationship with the person
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

        URI self = handleTypeAndAlias(a, vf, handler, FOAF.PERSON);

        if (a.getSharability() > 0.5) {
            handler.handleStatement(vf.createStatement(self, FOAF.NAME, vf.createLiteral(a.getValue())));
        } else {
            handler.handleStatement(vf.createStatement(self, RDFS.LABEL, vf.createLiteral(a.getValue())));
        }

        return self;
    }

    private static class NickHandler implements FieldHandler {
        @Override
        public void handle(Atom object, RDFizationContext context) throws RDFHandlerException {
            ValueFactory vf = context.getValueFactory();

            // TODO: this is an abuse of foaf:nick even when the domain is foaf:Person as it is here...
            // foaf:nick is supposed to be used for online handles, not aliases in general
            context.getHandler().handleStatement(
                    vf.createStatement(
                            context.getSubjectUri(), FOAF.NICK, vf.createLiteral(
                            AKAReference.extractAlias(object.getValue()))));
        }
    }

    private static class HomepageHandler implements FieldHandler {
        @Override
        public void handle(Atom object, RDFizationContext context) throws RDFHandlerException {
            ValueFactory vf = context.getValueFactory();
            URI objectURI = context.uriOf(object);
            context.getHandler().handleStatement(vf.createStatement(
                    context.getSubjectUri(), FOAF.HOMEPAGE, objectURI));
        }
    }

    private static class MadeHandler implements FieldHandler {
        @Override
        public void handle(Atom object, RDFizationContext context) throws RDFHandlerException {
            ValueFactory vf = context.getValueFactory();
            URI objectURI = context.uriOf(object);
            context.getHandler().handleStatement(vf.createStatement(
                    // note: dc:creator is recommended only for simple textual names
                    objectURI, FOAF.MAKER, context.getSubjectUri()));
        }
    }

    private static class QuotationHandler implements FieldHandler {
        @Override
        public void handle(Atom object, RDFizationContext context) throws RDFHandlerException {
            // TODO
        }
    }

    private static class InterestHandler implements FieldHandler {
        @Override
        public void handle(Atom object, RDFizationContext context) throws RDFHandlerException {
            ValueFactory vf = context.getValueFactory();
            URI objectURI = context.uriOf(object);
            context.getHandler().handleStatement(vf.createStatement(
                    context.getSubjectUri(), FOAF.INTEREST, objectURI));
        }
    }

    private static class KnowsHandler implements FieldHandler {
        @Override
        public void handle(Atom object, RDFizationContext context) throws RDFHandlerException {
            ValueFactory vf = context.getValueFactory();
            URI objectURI = context.uriOf(object);
            context.getHandler().handleStatement(vf.createStatement(
                    context.getSubjectUri(), FOAF.KNOWS, objectURI));
        }
    }

    private static class BirthdayHandler implements FieldHandler {
        @Override
        public void handle(Atom object, RDFizationContext context) throws RDFHandlerException {
            // technically, this is a misuse of foaf:birthday, which expects
            // string values of the form mm-dd, eg. '12-31', as opposed to
            // Extend-o-Brain's structured date values, which are represented
            // using the Event Ontology.
            // Nevertheless, foaf:birthday is used here because of its
            // appropriateness for the "birthday" meaning and its association with
            // foaf:Person, the RDF type associated with the EoB Person type.

            context.getHandler().handleStatement(
                    context.getValueFactory().createStatement(
                            context.getSubjectUri(), FOAF.BIRTHDAY, context.uriOf(object)));
        }
    }
}
