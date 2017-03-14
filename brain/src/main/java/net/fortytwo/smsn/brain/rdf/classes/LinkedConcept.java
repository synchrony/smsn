package net.fortytwo.smsn.brain.rdf.classes;

import net.fortytwo.smsn.brain.model.entities.Atom;
import net.fortytwo.smsn.brain.rdf.AtomClass;
import net.fortytwo.smsn.brain.rdf.AtomRegex;
import net.fortytwo.smsn.brain.rdf.RDFizationContext;
import net.fortytwo.smsn.brain.rdf.classes.collections.DocumentAboutTopicCollection;
import org.openrdf.model.IRI;
import org.openrdf.model.ValueFactory;
import org.openrdf.model.vocabulary.OWL;
import org.openrdf.model.vocabulary.RDFS;
import org.openrdf.rio.RDFHandler;
import org.openrdf.rio.RDFHandlerException;

import java.util.Arrays;
import java.util.regex.Pattern;

public class LinkedConcept extends AtomClass {

    public LinkedConcept() {
        super(
                "any-linked-concept",  // the name beginning with "a" gives this class a lexicographic advantage
                Pattern.compile("[a-zA-Z0-9].+"),
                // TODO: support concepts from datasets other than DBpedia
                Pattern.compile("http://dbpedia.org/resource/.+"),
                new AtomRegex(Arrays.asList(
                        new AtomRegex.El(new NickHandler(),
                                AtomRegex.Modifier.ZeroOrOne, AKAReference.class),
                        new AtomRegex.El(new PageHandler(),
                                AtomRegex.Modifier.ZeroOrMore, WebPage.class),

                        new AtomRegex.El(new DocumentsAboutTopicHandler(),
                                AtomRegex.Modifier.ZeroOrOne, DocumentAboutTopicCollection.class),

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

        IRI self = handleTypeAndAlias(a, context, OWL.THING);

        // note: we assume short, name-like values for linked atoms
        handler.handleStatement(vf.createStatement(self, RDFS.LABEL, vf.createLiteral(a.getTitle())));

        return self;
    }
}
