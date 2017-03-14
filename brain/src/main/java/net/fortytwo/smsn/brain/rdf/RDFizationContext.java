package net.fortytwo.smsn.brain.rdf;

import net.fortytwo.smsn.brain.model.entities.Atom;
import net.fortytwo.smsn.brain.model.TopicGraph;
import org.openrdf.model.IRI;
import org.openrdf.model.ValueFactory;
import org.openrdf.rio.RDFHandler;

public class RDFizationContext {
    private final TopicGraph topicGraph;
    private final RDFHandler handler;
    private final ValueFactory valueFactory;

    private IRI subjectUri;

    public RDFizationContext(final TopicGraph topicGraph,
                             final RDFHandler handler,
                             final ValueFactory valueFactory) {
        this.topicGraph = topicGraph;
        this.handler = handler;
        this.valueFactory = valueFactory;
    }

    public TopicGraph getTopicGraph() {
        return topicGraph;
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
        return valueFactory.createIRI(topicGraph.iriOfAtom(a));
    }
}
