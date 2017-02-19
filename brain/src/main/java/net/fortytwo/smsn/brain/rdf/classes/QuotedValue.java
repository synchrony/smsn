package net.fortytwo.smsn.brain.rdf.classes;

import net.fortytwo.smsn.brain.model.entities.Atom;
import net.fortytwo.smsn.brain.rdf.AtomClass;
import net.fortytwo.smsn.brain.rdf.RDFizationContext;
import net.fortytwo.smsn.rdf.vocab.SmSnVocabulary;
import org.openrdf.model.IRI;
import org.openrdf.model.ValueFactory;
import org.openrdf.model.vocabulary.RDFS;
import org.openrdf.rio.RDFHandler;
import org.openrdf.rio.RDFHandlerException;

import java.util.regex.Pattern;

public class QuotedValue extends AtomClass {

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
    public IRI toRDF(final Atom a,
                     final RDFizationContext context) throws RDFHandlerException {
        ValueFactory vf = context.getValueFactory();
        RDFHandler handler = context.getHandler();

        IRI self = handleTypeAndAlias(a, context, SmSnVocabulary.WORDORPHRASE);

        // note: a few atoms currently break this pattern, e.g. an atom with the value: "one", "two"
        String d = a.getTitle().substring(1, a.getTitle().length() - 1).trim();
        handler.handleStatement(vf.createStatement(self, RDFS.LABEL, vf.createLiteral(d)));

        return self;
    }
}
