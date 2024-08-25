package net.fortytwo.smsn.brain.model.pg;

import net.fortytwo.smsn.SemanticSynchrony;
import net.fortytwo.smsn.brain.AtomId;
import net.fortytwo.smsn.brain.model.entities.Topic;
import org.apache.tinkerpop.gremlin.structure.Direction;
import org.apache.tinkerpop.gremlin.structure.Vertex;

public abstract class PGTopic implements PGEntity, Topic {

    private final Vertex vertex;

    @Override
    public Vertex asVertex() {
        return vertex;
    }

    @Override
    public boolean equals(Object other) {
        return other instanceof PGTopic && PGEntity.equals(asVertex(), ((PGTopic) other).asVertex());
    }
    @Override
    public int hashCode() {
        return PGEntity.hashCode(asVertex());
    }

    protected PGTopic(Vertex vertex) {
        this.vertex = vertex;
    }

    @Override
    public AtomId getId() {
        return new AtomId(PGEntity.getRequiredProperty(asVertex(), SemanticSynchrony.PropertyKeys.ID));
    }

    @Override
    public void setId(final AtomId id) {
        AtomId noteId = null == id ? SemanticSynchrony.createRandomId() : id;
        PGEntity.setRequiredProperty(asVertex(), SemanticSynchrony.PropertyKeys.ID, noteId.value);
    }

    @Override
    public boolean isIsolated() {
        return !PGEntity.hasAdjacentVertex(asVertex(), SemanticSynchrony.EdgeLabels.TARGET, Direction.IN);
    }

    @Override
    public void destroy() {
        // nothing else to do; a topic owns no other entities
        PGEntity.destroyInternal(asVertex());
    }
}
