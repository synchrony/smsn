package net.fortytwo.smsn.brain.rdf;

import net.fortytwo.smsn.brain.Atom;
import net.fortytwo.smsn.brain.BrainGraph;
import org.openrdf.model.IRI;
import org.openrdf.model.ValueFactory;
import org.openrdf.rio.RDFHandler;

/**
 * @author Joshua Shinavier (http://fortytwo.net)
 */
public class RDFizationContext {
    private final RDFHandler handler;
    private final ValueFactory valueFactory;

    private IRI subjectUri;

    public RDFizationContext(final RDFHandler handler,
                             final ValueFactory valueFactory) {
        this.handler = handler;
        this.valueFactory = valueFactory;
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
        return valueFactory.createIRI(BrainGraph.iriOf(a));
    }
}
