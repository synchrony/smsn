package net.fortytwo.extendo.brain.rdf;

import net.fortytwo.extendo.brain.Atom;

/**
 * @author Joshua Shinavier (http://fortytwo.net)
 */
public interface Mapper {
    void mapToRDF(Atom parent, Atom child);
}
