package net.fortytwo.extendo.brain.rdf;

import net.fortytwo.extendo.brain.Atom;
import net.fortytwo.extendo.brain.BrainGraph;
import org.openrdf.model.URI;
import org.openrdf.model.ValueFactory;
import org.openrdf.rio.RDFHandler;

/**
 * @author Joshua Shinavier (http://fortytwo.net)
 */
public class RDFizationContext {
    private final RDFHandler handler;
    private final ValueFactory valueFactory;

    private URI subjectUri;

    public RDFizationContext(final RDFHandler handler,
                             final ValueFactory valueFactory) {
        this.handler = handler;
        this.valueFactory = valueFactory;
    }

    public void setSubject(Atom subject) {
        this.subjectUri = uriOf(subject);
    }

    public URI getSubjectUri() {
        return subjectUri;
    }

    public ValueFactory getValueFactory() {
        return valueFactory;
    }

    public RDFHandler getHandler() {
        return handler;
    }

    public URI uriOf(final Atom a) {
        return valueFactory.createURI(BrainGraph.uriOf(a));
    }
}
