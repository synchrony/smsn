package net.fortytwo.extendo.brain.rdf.classes;

import net.fortytwo.extendo.brain.Atom;
import net.fortytwo.extendo.brain.rdf.AtomClass;
import net.fortytwo.extendo.brain.rdf.AtomRegex;
import net.fortytwo.extendo.brain.rdf.RDFizationContext;
import net.fortytwo.extendo.brain.rdf.classes.collections.PersonCollection;
import net.fortytwo.extendo.rdf.vocab.FOAF;
import org.openrdf.model.URI;
import org.openrdf.model.ValueFactory;
import org.openrdf.model.vocabulary.DCTERMS;
import org.openrdf.model.vocabulary.OWL;
import org.openrdf.model.vocabulary.RDFS;
import org.openrdf.rio.RDFHandler;
import org.openrdf.rio.RDFHandlerException;

import java.util.Arrays;
import java.util.regex.Pattern;

/**
 * Something which is used for a purpose, such as hardware, software, or a service.
 *
 * @author Joshua Shinavier (http://fortytwo.net)
 */
public class Tool extends AtomClass {

    public Tool() {
        super(
                "tool",
                Pattern.compile("[a-zA-Z0-9].{1,49}"),
                null,
                new AtomRegex(Arrays.asList(
                        new AtomRegex.El(new AtomClass.NickHandler(),
                                AtomRegex.Modifier.ZeroOrOne, AKAReference.class),
                        new AtomRegex.El(new PageHandler(),
                                AtomRegex.Modifier.ZeroOrMore, WebPage.class),
                        new AtomRegex.El(null, // do nothing with usage for now
                                AtomRegex.Modifier.ZeroOrOne, Usage.class),
                        new AtomRegex.El(new ContributorHandler(),
                                AtomRegex.Modifier.ZeroOrOne, ContributorCollection.class),
                        new AtomRegex.El(null,
                                AtomRegex.Modifier.ZeroOrMore)
                )));
    }

    @Override
    protected boolean isCollectionClass() {
        return false;
    }

    private static class ContributorHandler implements FieldHandler {
        @Override
        public void handle(Atom object, RDFizationContext context) throws RDFHandlerException {
            ValueFactory vf = context.getValueFactory();
            URI objectURI = context.uriOf(object);
            context.getHandler().handleStatement(vf.createStatement(
                    // note: range of dcterms:contributor is dcterms:Agent, which is fairly broad
                    context.getSubjectUri(), DCTERMS.CONTRIBUTOR, objectURI));
        }
    }

    @Override
    public URI toRDF(Atom a, RDFizationContext context) throws RDFHandlerException {
        ValueFactory vf = context.getValueFactory();
        RDFHandler handler = context.getHandler();

        URI self = handleTypeAndAlias(a, vf, handler, OWL.THING);
        handler.handleStatement(vf.createStatement(self, RDFS.LABEL, vf.createLiteral(a.getValue())));

        return self;
    }

    public static class ContributorCollection extends PersonCollection {
        public ContributorCollection() {
            super();
            name = "contributor-collection";
            valueRegex = Pattern.compile("some (people|individuals) (involved in|who (have )?contribute to) .+");
        }
    }
}
