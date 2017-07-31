package net.fortytwo.smsn.brain.rdf.classes;

import net.fortytwo.smsn.brain.model.entities.Note;
import net.fortytwo.smsn.brain.rdf.NoteClass;
import net.fortytwo.smsn.brain.rdf.NoteReqex;
import net.fortytwo.smsn.brain.rdf.RDFizationContext;
import net.fortytwo.smsn.brain.rdf.classes.collections.PersonCollection;
import org.openrdf.model.IRI;
import org.openrdf.rio.RDFHandlerException;

import java.util.Arrays;
import java.util.regex.Pattern;

public class AbstractEvent extends NoteClass {
    public AbstractEvent() {
        super("abstract-event",
                Pattern.compile("[A-Z].+"),
                null,
                new NoteReqex(Arrays.asList(
                        new NoteReqex.El(new NickHandler(),
                                NoteReqex.Modifier.ZeroOrOne, AKAReference.class),
                        new NoteReqex.El(new PageHandler(),
                                NoteReqex.Modifier.ZeroOrMore, WebPage.class),
                        new NoteReqex.El(new InteractorHandler(),
                                NoteReqex.Modifier.ZeroOrOne, InteractorCollection.class),
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
        // TODO
        return null;
    }

    private static class InteractorHandler implements FieldHandler {
        @Override
        public void handle(Note object, RDFizationContext context) throws RDFHandlerException {
            // TODO
        }
    }

    public static class InteractorCollection extends PersonCollection {
        public InteractorCollection() {
            super();
            name = "interactor-collection";
            valueRegex = Pattern.compile("some people I .+ at .+");
        }
    }
}
