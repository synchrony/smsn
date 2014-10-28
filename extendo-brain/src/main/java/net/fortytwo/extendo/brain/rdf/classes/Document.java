package net.fortytwo.extendo.brain.rdf.classes;

import net.fortytwo.extendo.brain.Atom;
import net.fortytwo.extendo.brain.rdf.AtomClass;
import net.fortytwo.extendo.brain.rdf.AtomRegex;
import net.fortytwo.extendo.brain.rdf.RDFizationContext;
import net.fortytwo.extendo.brain.rdf.classes.collections.NoteCollection;
import net.fortytwo.extendo.brain.rdf.classes.collections.PersonCollection;
import net.fortytwo.extendo.rdf.vocab.DCTerms;
import net.fortytwo.extendo.rdf.vocab.FOAF;
import org.openrdf.model.URI;
import org.openrdf.model.ValueFactory;
import org.openrdf.rio.RDFHandler;
import org.openrdf.rio.RDFHandlerException;

import java.util.Arrays;
import java.util.regex.Pattern;

/**
 * @author Joshua Shinavier (http://fortytwo.net)
 */
public class Document extends AtomClass {
    public static final Document INSTANCE = new Document();

    public Document() {
        super(
                "document",
                Pattern.compile("[A-Z].+"),
                null,
                new AtomRegex(Arrays.asList(
                        new AtomRegex.El(new ISBNHandler(),
                                AtomRegex.Modifier.ZeroOrOne, ISBNReference.class),
                        new AtomRegex.El(new BibtexHandler(),
                                AtomRegex.Modifier.ZeroOrOne, BibtexReference.class),
                        new AtomRegex.El(new RFIDHandler(),
                                AtomRegex.Modifier.ZeroOrOne, RFIDReference.class),
                        new AtomRegex.El(new MakerHandler(),
                                AtomRegex.Modifier.ZeroOrOne, PersonCollection.class),
                        new AtomRegex.El(new NoteHandler(),
                                AtomRegex.Modifier.ZeroOrOne, NoteCollection.class),
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

        // TODO: a more specific type than foaf:Document may be appropriate (WebPage also uses foaf:Document)
        URI self = handleTypeAndAlias(a, vf, handler, FOAF.DOCUMENT);

        handler.handleStatement(vf.createStatement(self, DCTerms.TITLE, vf.createLiteral(a.getValue())));

        return self;
    }

    private static class ISBNHandler implements FieldHandler {
        @Override
        public void handle(Atom object, RDFizationContext context) throws RDFHandlerException {
            // TODO
        }
    }

    private static class BibtexHandler implements FieldHandler {
        @Override
        public void handle(Atom object, RDFizationContext context) throws RDFHandlerException {
            // TODO
        }
    }

    private static class RFIDHandler implements FieldHandler {
        @Override
        public void handle(Atom object, RDFizationContext context) throws RDFHandlerException {
            // TODO
        }
    }

    private static class MakerHandler implements FieldHandler {
        @Override
        public void handle(Atom object, RDFizationContext context) throws RDFHandlerException {
            ValueFactory vf = context.getValueFactory();
            URI objectURI = context.uriOf(object);
            context.getHandler().handleStatement(vf.createStatement(
                    // note: dc:creator is recommended only for simple textual names
                    context.getSubjectUri(), FOAF.MAKER, objectURI));
        }
    }

    private static class NoteHandler implements FieldHandler {
        @Override
        public void handle(Atom object, RDFizationContext context) throws RDFHandlerException {
            // TODO
        }
    }
}
