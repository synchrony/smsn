package net.fortytwo.smsn.brain.rdf.classes;

import net.fortytwo.smsn.brain.model.Atom;
import net.fortytwo.smsn.brain.rdf.AtomClass;
import net.fortytwo.smsn.brain.rdf.AtomRegex;
import net.fortytwo.smsn.brain.rdf.RDFizationContext;
import net.fortytwo.smsn.brain.rdf.classes.collections.DocumentAboutTopicCollection;
import net.fortytwo.smsn.brain.rdf.classes.collections.PersonCollection;
import net.fortytwo.smsn.rdf.vocab.FOAF;
import org.openrdf.model.IRI;
import org.openrdf.model.ValueFactory;
import org.openrdf.model.vocabulary.DCTERMS;
import org.openrdf.rio.RDFHandler;
import org.openrdf.rio.RDFHandlerException;

import java.util.Arrays;
import java.util.logging.Logger;
import java.util.regex.Pattern;

/**
 * @author Joshua Shinavier (http://fortytwo.net)
 */
public class Organization extends AtomClass {
    private static final Logger logger = Logger.getLogger(Organization.class.getName());

    public Organization() {
        super(
                "organization",
                Pattern.compile("[A-Z].+"),
                null,
                new AtomRegex(Arrays.asList(
                        new AtomRegex.El(new NickHandler(),
                                AtomRegex.Modifier.ZeroOrOne, AKAReference.class),
                        new AtomRegex.El(new PageHandler(),
                                AtomRegex.Modifier.ZeroOrMore, WebPage.class),

                        new AtomRegex.El(new DocumentsAboutTopicHandler(),
                                AtomRegex.Modifier.ZeroOrOne, DocumentAboutTopicCollection.class),

                        new AtomRegex.El(new MemberHandler(),
                                AtomRegex.Modifier.ZeroOrOne, PersonCollection.class),

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

        IRI self = handleTypeAndAlias(a, context, FOAF.ORGANIZATION);

        handler.handleStatement(vf.createStatement(self, DCTERMS.TITLE, vf.createLiteral(a.getValue())));

        return self;
    }

    public static class MemberHandler implements FieldHandler {
        @Override
        public void handle(Atom object, RDFizationContext context) throws RDFHandlerException {
            ValueFactory vf = context.getValueFactory();
            IRI objectIRI = context.iriOf(object);
            context.getHandler().handleStatement(vf.createStatement(
                    context.getSubjectIri(), FOAF.MEMBER, objectIRI));
        }
    }
}
