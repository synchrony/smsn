package net.fortytwo.smsn.brain.rdf.classes;

import net.fortytwo.smsn.brain.model.entities.Note;
import net.fortytwo.smsn.brain.rdf.NoteClass;
import net.fortytwo.smsn.brain.rdf.NoteReqex;
import net.fortytwo.smsn.brain.rdf.RDFizationContext;
import net.fortytwo.smsn.brain.rdf.classes.collections.DocumentAboutTopicCollection;
import org.eclipse.rdf4j.model.IRI;
import org.eclipse.rdf4j.model.ValueFactory;
import org.eclipse.rdf4j.model.vocabulary.DCTERMS;
import org.eclipse.rdf4j.model.vocabulary.OWL;
import org.eclipse.rdf4j.rio.RDFHandler;
import org.eclipse.rdf4j.rio.RDFHandlerException;

import java.util.Arrays;
import java.util.logging.Logger;
import java.util.regex.Pattern;

public class Topic extends NoteClass {
    private static final Logger logger = Logger.getLogger(Topic.class.getName());

    public Topic() {
        super(
                "topic",
                Pattern.compile("[A-Z].+"),
                null,
                new NoteReqex(Arrays.asList(
                        new NoteReqex.El(new NickHandler(),
                                NoteReqex.Modifier.ZeroOrOne, AKAReference.class),
                        new NoteReqex.El(new PageHandler(),
                                NoteReqex.Modifier.ZeroOrMore, WebPage.class),

                        new NoteReqex.El(new DocumentsAboutTopicHandler(),
                                NoteReqex.Modifier.ZeroOrOne, DocumentAboutTopicCollection.class),

                        // multiple RFID tags on an object are possible, though they may be uncommon
                        new NoteReqex.El(2, new RFIDHandler(),
                                NoteReqex.Modifier.ZeroOrMore, RFIDReference.class),

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

        handler.handleStatement(vf.createStatement(self, DCTERMS.TITLE, vf.createLiteral(Note.getTitle(a))));

        return self;
    }
}
