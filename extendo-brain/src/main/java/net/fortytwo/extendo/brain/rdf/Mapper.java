package net.fortytwo.extendo.brain.rdf;

import net.fortytwo.extendo.brain.Atom;
import org.openrdf.model.URI;
import org.openrdf.model.ValueFactory;
import org.openrdf.rio.RDFHandler;
import org.openrdf.rio.RDFHandlerException;

/**
 * @author Joshua Shinavier (http://fortytwo.net)
 */
public interface Mapper {
    void mapToRDF(Atom parent,
                  Atom child,
                  URI parentUri,
                  ValueFactory vf,
                  RDFHandler handler) throws RDFHandlerException;
}
