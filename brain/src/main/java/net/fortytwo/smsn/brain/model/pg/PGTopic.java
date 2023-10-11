package net.fortytwo.smsn.brain.model.pg;

import net.fortytwo.smsn.SemanticSynchrony;
import net.fortytwo.smsn.brain.AtomId;
import net.fortytwo.smsn.brain.model.entities.Topic;
import org.apache.tinkerpop.gremlin.structure.Direction;
import org.apache.tinkerpop.gremlin.structure.Vertex;

public abstract class PGTopic extends PGEntity implements Topic {

    protected PGTopic(Vertex vertex) {
        super(vertex);
    }

    @Override
    public AtomId getId() {
        return new AtomId(getRequiredProperty(SemanticSynchrony.PropertyKeys.ID));
    }

    @Override
    public void setId(final AtomId id) {
        AtomId noteId = null == id ? SemanticSynchrony.createRandomId() : id;
        setRequiredProperty(SemanticSynchrony.PropertyKeys.ID, noteId.value);
    }

    @Override
    public boolean isIsolated() {
        return !hasAdjacentVertex(SemanticSynchrony.EdgeLabels.TARGET, Direction.IN);
    }

    @Override
    public void destroy() {
        // nothing else to do; a topic owns no other entities
        destroyInternal();
    }
}
