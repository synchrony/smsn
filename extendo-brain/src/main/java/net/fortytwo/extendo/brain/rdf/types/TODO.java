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
public class TODO extends SimpleType {
    public static final TODO INSTANCE = new TODO();

    private TODO() {
        super("todo");
    }

    public boolean additionalConstraintsSatisfied(final String value) {
        return true;
    }

    public Pattern getValueRegex() {
        return Pattern.compile("TODO: .+");
    }

    public URI translateToRDF(final Atom a,
                               final ValueFactory vf,
                               final RDFHandler handler) throws RDFHandlerException {
        URI self = translateTypeAndAlias(a, vf, handler, ExtendoVocab.TODO);

        // assumes the prefix "TODO:"
        String d = a.getValue().substring(5).trim();
        handler.handleStatement(vf.createStatement(self, RDFS.COMMENT, vf.createLiteral(d)));

        return self;
    }
}
