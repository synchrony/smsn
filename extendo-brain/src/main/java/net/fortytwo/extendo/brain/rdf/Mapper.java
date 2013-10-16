package net.fortytwo.extendo.brain.rdf;

import net.fortytwo.extendo.brain.Atom;
import org.openrdf.rio.RDFHandlerException;

/**
 * @author Joshua Shinavier (http://fortytwo.net)
 */
public interface Mapper {
    void mapToRDF(Atom child,
                  MappingContext context) throws RDFHandlerException;
}
