package net.fortytwo.smsn.brain.rdf;

import net.fortytwo.smsn.brain.model.Atom;
import net.fortytwo.smsn.brain.model.AtomGraph;
import org.openrdf.model.IRI;
import org.openrdf.model.ValueFactory;
import org.openrdf.rio.RDFHandler;

/**
 * @author Joshua Shinavier (http://fortytwo.net)
 */
public class RDFizationContext {
    private final AtomGraph atomGraph;
    private final RDFHandler handler;
    private final ValueFactory valueFactory;

    private IRI subjectUri;

    public RDFizationContext(final AtomGraph atomGraph,
                             final RDFHandler handler,
                             final ValueFactory valueFactory) {
        this.atomGraph = atomGraph;
        this.handler = handler;
        this.valueFactory = valueFactory;
    }

    public AtomGraph getAtomGraph() {
        return atomGraph;
    }

    public void setSubject(Atom subject) {
        this.subjectUri = iriOf(subject);
    }

    public IRI getSubjectIri() {
        return subjectUri;
    }

    public ValueFactory getValueFactory() {
        return valueFactory;
    }

    public RDFHandler getHandler() {
        return handler;
    }

    public IRI iriOf(final Atom a) {
        return valueFactory.createIRI(atomGraph.iriOfAtom(a));
    }
}
