package net.fortytwo.smsn.brain.model.pg;

import net.fortytwo.smsn.SemanticSynchrony;
import net.fortytwo.smsn.brain.model.entities.Link;
import net.fortytwo.smsn.brain.model.entities.Page;
import net.fortytwo.smsn.brain.model.entities.Topic;
import org.apache.tinkerpop.gremlin.structure.Direction;
import org.apache.tinkerpop.gremlin.structure.Vertex;

import java.util.function.Consumer;

public abstract class PGTopic extends PGEntity implements Topic {

    protected PGTopic(Vertex vertex) {
        super(vertex);
    }

    @Override
    public String getId() {
        return super.getId();
    }

    @Override
    public boolean setId(final String id) {
        String atomId = null == id ? SemanticSynchrony.createRandomId() : id;
        return setRequiredProperty(SemanticSynchrony.PropertyKeys.ID_V, atomId);
    }

    @Override
    public Page getPrimaryTopicOf() {
        return getAtMostOneEntity(SemanticSynchrony.EdgeLabels.TOPIC, Direction.IN, vertex -> getGraph().asPage(vertex));
    }

    @Override
    public void forTargetOf(Consumer<Link> consumer) {
        forEachAdjacentVertex(SemanticSynchrony.EdgeLabels.TARGET, Direction.IN,
                vertex -> consumer.accept(getGraph().asLink(vertex)));
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
