package net.fortytwo.smsn.brain.rdf;

import net.fortytwo.smsn.brain.model.Atom;
import org.openrdf.model.IRI;
import org.openrdf.rio.RDFHandlerException;

import java.util.regex.Pattern;

public abstract class AtomCollection extends AtomClass {

    public AtomCollection(final String name,
                          final Pattern valueRegex,
                          final Pattern aliasRegex,
                          final AtomRegex memberRegex) {
        super(name, valueRegex, aliasRegex, memberRegex);
    }

    @Override
    protected boolean isCollectionClass() {
        return true;
    }

    public IRI toRDF(final Atom a,
                     final RDFizationContext context) throws RDFHandlerException {
        // Collections do not have URIs and do not have associated RDF statements.
        // Instead, statements are distributed over the members of the collection.
        return null;
    }
}
