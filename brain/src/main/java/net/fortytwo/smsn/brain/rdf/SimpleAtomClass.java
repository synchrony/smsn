package net.fortytwo.smsn.brain.rdf;

import net.fortytwo.smsn.brain.model.entities.Note;
import org.openrdf.model.IRI;
import org.openrdf.rio.RDFHandlerException;

import java.util.regex.Pattern;

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

    public IRI toRDF(final Note a,
                     final RDFizationContext context) throws RDFHandlerException {
        // Some classes are meaningful only as fields of other classes.
        // They are not identified by URIs, and they are not the subjects of RDF statements.
        return null;
    }
}
