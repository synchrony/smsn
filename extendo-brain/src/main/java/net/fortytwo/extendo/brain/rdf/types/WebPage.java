package net.fortytwo.extendo.brain.rdf.types;

import net.fortytwo.extendo.brain.Atom;
import net.fortytwo.extendo.brain.rdf.BottomUpType;
import net.fortytwo.extendo.brain.rdf.Field;
import net.fortytwo.extendo.rdf.vocab.DCTerms;
import net.fortytwo.extendo.rdf.vocab.FOAF;
import org.openrdf.model.URI;
import org.openrdf.model.ValueFactory;
import org.openrdf.rio.RDFHandler;
import org.openrdf.rio.RDFHandlerException;

import java.util.regex.Pattern;

/**
 * @author Joshua Shinavier (http://fortytwo.net)
 */
public class WebPage extends BottomUpType {
    public static final WebPage INSTANCE = new WebPage();

    private Field[] fields = null;

    private WebPage() {
    }

    public boolean additionalConstraintsSatisfied(final String value) {
        return true;
    }

    public Field[] getFields() {
        if (null == fields) {
            fields = new Field[]{};
        }

        return fields;
    }

    public Pattern getValueRegex() {
        return Pattern.compile(".+ \\(web page\\)");
    }

    public boolean childrenRequired() {
        return false;
    }

    public boolean aliasRequired() {
        return true;
    }

    public URI translateToRDF(final Atom a,
                               final ValueFactory vf,
                               final RDFHandler handler) throws RDFHandlerException {
        URI self = translateTypeAndAlias(a, vf, handler, FOAF.DOCUMENT);

        // note: assumes that the value ends with "(web page)"
        int i = a.getValue().lastIndexOf("(");
        String d = a.getValue().substring(0, i).trim();
        handler.handleStatement(vf.createStatement(self, DCTerms.TITLE, vf.createLiteral(d)));

        return self;
    }
}
