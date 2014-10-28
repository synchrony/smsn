package net.fortytwo.extendo.brain.rdf.classes;

import net.fortytwo.extendo.brain.Atom;
import net.fortytwo.extendo.brain.rdf.AtomClass;
import net.fortytwo.extendo.brain.rdf.RDFizationContext;
import net.fortytwo.extendo.rdf.vocab.ExtendoVocab;
import org.openrdf.model.URI;
import org.openrdf.model.ValueFactory;
import org.openrdf.model.vocabulary.RDFS;
import org.openrdf.rio.RDFHandler;
import org.openrdf.rio.RDFHandlerException;

import java.util.regex.Pattern;

/**
 * @author Joshua Shinavier (http://fortytwo.net)
 */
public class QuotedValue extends AtomClass {
    public static final QuotedValue INSTANCE = new QuotedValue();

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
    public URI toRDF(final Atom a,
                     final RDFizationContext context) throws RDFHandlerException {
        ValueFactory vf = context.getValueFactory();
        RDFHandler handler = context.getHandler();

        URI self = handleTypeAndAlias(a, vf, handler, ExtendoVocab.WORDORPHRASE);

        // note: a few atoms currently break this pattern, e.g. an atom with the value: "one", "two"
        String d = a.getValue().substring(1, a.getValue().length() - 1).trim();
        handler.handleStatement(vf.createStatement(self, RDFS.LABEL, vf.createLiteral(d)));

        return self;
    }
}
