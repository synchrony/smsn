package net.fortytwo.smsn.brain.rdf.classes;

import net.fortytwo.smsn.brain.model.entities.Note;
import net.fortytwo.smsn.brain.rdf.NoteClass;
import net.fortytwo.smsn.brain.rdf.RDFizationContext;
import net.fortytwo.smsn.rdf.vocab.SmSnVocabulary;
import org.openrdf.model.IRI;
import org.openrdf.model.ValueFactory;
import org.openrdf.model.vocabulary.RDFS;
import org.openrdf.rio.RDFHandler;
import org.openrdf.rio.RDFHandlerException;

import java.util.regex.Pattern;

public class QuotedValue extends NoteClass {

    public QuotedValue() {
        super(
                "quoted-value",
                Pattern.compile("\\\".+\\\""),
                null,
                null
                );
    }

    @Override
    protected boolean isCollectionClass() {
        return false;
    }

    @Override
    public IRI toRDF(final Note a,
                     final RDFizationContext context) throws RDFHandlerException {
        ValueFactory vf = context.getValueFactory();
        RDFHandler handler = context.getHandler();

        IRI self = handleTypeAndAlias(a, context, SmSnVocabulary.WORDORPHRASE);

        // note: a few notes currently break this pattern, e.g. a note with the value: "one", "two"
        String d = Note.getTitle(a).substring(1, Note.getTitle(a).length() - 1).trim();
        handler.handleStatement(vf.createStatement(self, RDFS.LABEL, vf.createLiteral(d)));

        return self;
    }
}
