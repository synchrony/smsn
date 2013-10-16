package net.fortytwo.extendo.brain.rdf.types;

import net.fortytwo.extendo.brain.Atom;
import org.openrdf.model.URI;
import org.openrdf.model.ValueFactory;
import org.openrdf.rio.RDFHandler;
import org.openrdf.rio.RDFHandlerException;

/**
 * @author Joshua Shinavier (http://fortytwo.net)
 */
public abstract class SimpleSecondClassType extends SimpleType {
    public URI translateToRDF(final Atom a,
                               final ValueFactory vf,
                               final RDFHandler handler) throws RDFHandlerException {
        // Second-class types exist only as fields of other notes.
        // They are not identified by URIs, and they are not the subjects of RDF statements.
        return null;
    }
}
