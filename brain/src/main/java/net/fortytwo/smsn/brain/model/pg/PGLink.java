package net.fortytwo.smsn.brain.model.pg;

import net.fortytwo.smsn.SemanticSynchrony;
import net.fortytwo.smsn.brain.model.entities.Link;
import net.fortytwo.smsn.brain.model.entities.Page;
import net.fortytwo.smsn.brain.model.entities.Topic;
import org.apache.tinkerpop.gremlin.structure.Direction;
import org.apache.tinkerpop.gremlin.structure.Vertex;

public abstract class PGLink extends PGEntity implements Link {

    protected PGLink(Vertex vertex) {
        super(vertex);
    }

    @Override
    public Topic getTarget() {
        return getExactlyOneEntity(SemanticSynchrony.EdgeLabels.TARGET, Direction.OUT,
                vertex -> getGraph().asTopic(vertex));
    }

    @Override
    public boolean setTarget(Topic target) {
        return setRequiredEntity(SemanticSynchrony.EdgeLabels.TARGET, target);
    }

    @Override
    public String getLabel() {
        return getRequiredProperty(SemanticSynchrony.PropertyKeys.LABEL);
    }

    @Override
    public boolean setLabel(String label) {
        return setRequiredProperty(SemanticSynchrony.PropertyKeys.LABEL, label);
    }

    @Override
    public Page getContext() {
        return getExactlyOneEntity(SemanticSynchrony.EdgeLabels.CONTEXT, Direction.OUT, vertex -> getGraph().asPage(vertex));
    }

    @Override
    public boolean setContext(Page context) {
        return setRequiredEntity(SemanticSynchrony.EdgeLabels.CONTEXT, context);
    }

    @Override
    public void destroy() {
        Topic target = getTarget();

        destroyInternal();

        // a link owns its target only if there are no other links to the target
        if (target.isIsolated()) {
            target.destroy();
        }
    }
}
