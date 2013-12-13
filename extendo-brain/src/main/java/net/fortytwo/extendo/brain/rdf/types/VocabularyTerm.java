package net.fortytwo.extendo.brain.rdf.types;

import net.fortytwo.extendo.brain.Atom;
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
public class VocabularyTerm extends SimpleType {
    public static final VocabularyTerm INSTANCE = new VocabularyTerm();

    private VocabularyTerm() {
        super("vocabulary-term");
    }

    public Pattern getValueRegex() {
        return Pattern.compile("\\\".+\\\"");
    }

    public boolean additionalConstraintsSatisfied(final String value) {
        return true;
    }

    public URI translateToRDF(final Atom a,
                               final ValueFactory vf,
                               final RDFHandler handler) throws RDFHandlerException {
        URI self = translateTypeAndAlias(a, vf, handler, ExtendoVocab.WORDORPHRASE);

        // note: a few atoms currently break this pattern, e.g. an atom with the value: "one", "two"
        String d = a.getValue().substring(1, a.getValue().length() - 1).trim();
        handler.handleStatement(vf.createStatement(self, RDFS.LABEL, vf.createLiteral(d)));

        return self;
    }
}
