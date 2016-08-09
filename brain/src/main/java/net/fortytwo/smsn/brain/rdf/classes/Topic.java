package net.fortytwo.smsn.brain.rdf.classes;

import net.fortytwo.smsn.brain.model.Atom;
import net.fortytwo.smsn.brain.rdf.AtomClass;
import net.fortytwo.smsn.brain.rdf.AtomRegex;
import net.fortytwo.smsn.brain.rdf.RDFizationContext;
import net.fortytwo.smsn.brain.rdf.classes.collections.DocumentAboutTopicCollection;
import org.openrdf.model.IRI;
import org.openrdf.model.ValueFactory;
import org.openrdf.model.vocabulary.DCTERMS;
import org.openrdf.model.vocabulary.OWL;
import org.openrdf.rio.RDFHandler;
import org.openrdf.rio.RDFHandlerException;

import java.util.Arrays;
import java.util.logging.Logger;
import java.util.regex.Pattern;

public class Topic extends AtomClass {
    private static final Logger logger = Logger.getLogger(Topic.class.getName());

    public Topic() {
        super(
                "topic",
                Pattern.compile("[A-Z].+"),
                null,
                new AtomRegex(Arrays.asList(
                        new AtomRegex.El(new NickHandler(),
                                AtomRegex.Modifier.ZeroOrOne, AKAReference.class),
                        new AtomRegex.El(new PageHandler(),
                                AtomRegex.Modifier.ZeroOrMore, WebPage.class),

                        new AtomRegex.El(new DocumentsAboutTopicHandler(),
                                AtomRegex.Modifier.ZeroOrOne, DocumentAboutTopicCollection.class),

                        // multiple RFID tags on an object are possible, though they may be uncommon
                        new AtomRegex.El(2, new RFIDHandler(),
                                AtomRegex.Modifier.ZeroOrMore, RFIDReference.class),

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

        handler.handleStatement(vf.createStatement(self, DCTERMS.TITLE, vf.createLiteral(a.getValue())));

        return self;
    }
}
