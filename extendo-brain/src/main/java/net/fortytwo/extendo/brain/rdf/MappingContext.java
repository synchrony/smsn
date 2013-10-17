package net.fortytwo.extendo.brain.rdf;

import net.fortytwo.extendo.brain.Atom;
import net.fortytwo.extendo.brain.BrainGraph;
import org.openrdf.model.URI;
import org.openrdf.model.ValueFactory;
import org.openrdf.rio.RDFHandler;

/**
* @author Joshua Shinavier (http://fortytwo.net)
*/
public class MappingContext {
    private Atom reference;
    private URI referenceUri;
    private ValueFactory valueFactory;
    private RDFHandler handler;
    private final KnowledgeBase knowledgeBase;

    public MappingContext(final KnowledgeBase knowledgeBase) {
        this.knowledgeBase = knowledgeBase;
    }

    public Atom getReference() {
        return reference;
    }

    public void setReference(Atom reference) {
        this.reference = reference;
    }

    public URI getReferenceUri() {
        return referenceUri;
    }

    public void setReferenceUri(URI referenceUri) {
        this.referenceUri = referenceUri;
    }

    public ValueFactory getValueFactory() {
        return valueFactory;
    }

    public void setValueFactory(ValueFactory valueFactory) {
        this.valueFactory = valueFactory;
    }

    public RDFHandler getHandler() {
        return handler;
    }

    public void setHandler(RDFHandler handler) {
        this.handler = handler;
    }

    public URI uriOf(final Atom a) {
        return valueFactory.createURI(BrainGraph.uriOf(a));
    }

    public KnowledgeBase getKnowledgeBase() {
        return knowledgeBase;
    }
}
