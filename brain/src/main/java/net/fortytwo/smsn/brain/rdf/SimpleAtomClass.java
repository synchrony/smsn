package net.fortytwo.smsn.brain.rdf;

import net.fortytwo.smsn.brain.Atom;
import org.openrdf.model.IRI;
import org.openrdf.rio.RDFHandlerException;

import java.util.regex.Pattern;

/**
 * @author Joshua Shinavier (http://fortytwo.net)
 */
public abstract class SimpleAtomClass extends AtomClass {

    public SimpleAtomClass(final String name,
                           final Pattern valueRegex,
                           final Pattern aliasRegex,
                           final AtomRegex memberRegex) {
        super(name, valueRegex, aliasRegex, memberRegex);
    }

    @Override
    protected boolean isCollectionClass() {
        return false;
    }

    public IRI toRDF(final Atom a,
                     final RDFizationContext context) throws RDFHandlerException {
        // Some classes are meaningful only as fields of other classes.
        // They are not identified by URIs, and they are not the subjects of RDF statements.
        return null;
    }
}
