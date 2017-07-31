package net.fortytwo.smsn.brain.rdf.classes;

import net.fortytwo.smsn.brain.model.entities.Note;
import net.fortytwo.smsn.brain.rdf.NoteClass;
import net.fortytwo.smsn.brain.rdf.NoteReqex;
import net.fortytwo.smsn.brain.rdf.RDFizationContext;
import net.fortytwo.smsn.brain.rdf.classes.collections.DocumentAboutTopicCollection;
import net.fortytwo.smsn.brain.rdf.classes.collections.PersonCollection;
import org.openrdf.model.IRI;
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
 */
public class Tool extends NoteClass {

    public Tool() {
        super(
                "tool",
                Pattern.compile("[a-zA-Z0-9].{1,49}"),
                null,
                new NoteReqex(Arrays.asList(
                        new NoteReqex.El(new NoteClass.NickHandler(),
                                NoteReqex.Modifier.ZeroOrOne, AKAReference.class),
                        new NoteReqex.El(new PageHandler(),
                                NoteReqex.Modifier.ZeroOrMore, WebPage.class),

                        new NoteReqex.El(new DocumentsAboutTopicHandler(),
                                NoteReqex.Modifier.ZeroOrOne, DocumentAboutTopicCollection.class),

                        new NoteReqex.El(null, // do nothing with usage for now
                                NoteReqex.Modifier.ZeroOrOne, Usage.class),
                        new NoteReqex.El(new ContributorHandler(),
                                NoteReqex.Modifier.ZeroOrOne, ContributorCollection.class),
                        new NoteReqex.El(null,
                                NoteReqex.Modifier.ZeroOrMore)
                )));
    }

    @Override
    protected boolean isCollectionClass() {
        return false;
    }

    private static class ContributorHandler implements FieldHandler {
        @Override
        public void handle(Note object, RDFizationContext context) throws RDFHandlerException {
            ValueFactory vf = context.getValueFactory();
            IRI objectURI = context.iriOf(object);
            context.getHandler().handleStatement(vf.createStatement(
                    // note: range of dcterms:contributor is dcterms:Agent, which is fairly broad
                    context.getSubjectIri(), DCTERMS.CONTRIBUTOR, objectURI));
        }
    }

    @Override
    public IRI toRDF(Note a, RDFizationContext context) throws RDFHandlerException {
        ValueFactory vf = context.getValueFactory();
        RDFHandler handler = context.getHandler();

        IRI self = handleTypeAndAlias(a, context, OWL.THING);
        handler.handleStatement(vf.createStatement(self, RDFS.LABEL, vf.createLiteral(Note.getTitle(a))));

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
