package net.fortytwo.extendo.brain.rdf.types;

import net.fortytwo.extendo.brain.Atom;
import net.fortytwo.extendo.brain.rdf.BottomUpType;
import net.fortytwo.extendo.brain.rdf.Field;
import org.openrdf.model.ValueFactory;
import org.openrdf.rio.RDFHandler;
import org.openrdf.rio.RDFHandlerException;

import java.util.regex.Pattern;

/**
 * @author Joshua Shinavier (http://fortytwo.net)
 */
public class OpenCollection extends BottomUpType {
    public static final OpenCollection INSTANCE = new OpenCollection();

    private final Field[] fields = new Field[]{};

    private OpenCollection() {
    }

    public Field[] getFields() {
        return fields;
    }

    public Pattern getValueRegex() {
        return Pattern.compile("some .+");
    }

    public boolean additionalConstraintsSatisfied(final String value) {
        return true;
    }

    public boolean childrenRequired() {
        return true;
    }

    public boolean aliasRequired() {
        return false;
    }

    public void translateToRDF(final Atom a,
                               final ValueFactory vf,
                               final RDFHandler handler) throws RDFHandlerException {
        // do nothing; the rdfization of collections is context-specific, and collection values are discarded
    }
}
