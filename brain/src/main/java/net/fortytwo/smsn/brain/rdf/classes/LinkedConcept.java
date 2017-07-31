package net.fortytwo.smsn.brain.rdf.classes;

import net.fortytwo.smsn.brain.model.entities.Note;
import net.fortytwo.smsn.brain.rdf.NoteClass;
import net.fortytwo.smsn.brain.rdf.NoteReqex;
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

public class LinkedConcept extends NoteClass {

    public LinkedConcept() {
        super(
                "any-linked-concept",  // the name beginning with "a" gives this class a lexicographic advantage
                Pattern.compile("[a-zA-Z0-9].+"),
                // TODO: support concepts from datasets other than DBpedia
                Pattern.compile("http://dbpedia.org/resource/.+"),
                new NoteReqex(Arrays.asList(
                        new NoteReqex.El(new NickHandler(),
                                NoteReqex.Modifier.ZeroOrOne, AKAReference.class),
                        new NoteReqex.El(new PageHandler(),
                                NoteReqex.Modifier.ZeroOrMore, WebPage.class),

                        new NoteReqex.El(new DocumentsAboutTopicHandler(),
                                NoteReqex.Modifier.ZeroOrOne, DocumentAboutTopicCollection.class),

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

        IRI self = handleTypeAndAlias(a, context, OWL.THING);

        // note: we assume short, name-like values for linked notes
        handler.handleStatement(vf.createStatement(self, RDFS.LABEL, vf.createLiteral(Note.getTitle(a))));

        return self;
    }
}
