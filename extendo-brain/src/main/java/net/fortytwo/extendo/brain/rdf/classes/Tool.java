package net.fortytwo.extendo.brain.rdf.classes;

import net.fortytwo.extendo.brain.Atom;
import net.fortytwo.extendo.brain.rdf.AtomClass;
import net.fortytwo.extendo.brain.rdf.AtomRegex;
import net.fortytwo.extendo.brain.rdf.RDFizationContext;
import net.fortytwo.extendo.rdf.vocab.FOAF;
import org.openrdf.model.URI;
import org.openrdf.model.ValueFactory;
import org.openrdf.model.vocabulary.OWL;
import org.openrdf.model.vocabulary.RDFS;
import org.openrdf.rio.RDFHandler;
import org.openrdf.rio.RDFHandlerException;

import java.util.Arrays;
import java.util.regex.Pattern;

/**
 * Something which is used for a purpose, including hardware, software, and services.
 *
 * @author Joshua Shinavier (http://fortytwo.net)
 */
public class Tool extends AtomClass {

    public Tool() {
        super(
                "tool",
                Pattern.compile(".{1,50}"),
                null,
                new AtomRegex(Arrays.asList(
                        new AtomRegex.El(new NickHandler(),
                                AtomRegex.Modifier.ZeroOrOne, AKAReference.class),
                        new AtomRegex.El(new HomepageHandler(),
                                AtomRegex.Modifier.ZeroOrMore, WebPage.class),
                        new AtomRegex.El(null, // do nothing with usage for now
                                AtomRegex.Modifier.ZeroOrOne, Usage.class),
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

        URI self = handleTypeAndAlias(a, vf, handler, OWL.THING);
        handler.handleStatement(vf.createStatement(self, RDFS.LABEL, vf.createLiteral(a.getValue())));

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
}
