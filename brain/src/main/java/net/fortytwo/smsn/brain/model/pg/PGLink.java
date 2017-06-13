package net.fortytwo.smsn.brain.model.pg;

import net.fortytwo.smsn.SemanticSynchrony;
import net.fortytwo.smsn.brain.model.Role;
import net.fortytwo.smsn.brain.model.entities.Link;
import net.fortytwo.smsn.brain.model.entities.Topic;
import org.apache.tinkerpop.gremlin.structure.Direction;
import org.apache.tinkerpop.gremlin.structure.Vertex;

public abstract class PGLink extends PGEntity implements Link {

    protected PGLink(Vertex vertex) {
        super(vertex);
    }

    @Override
    public Role getRole() {
        return Role.valueOf(getRequiredProperty(SemanticSynchrony.PropertyKeys.ROLE));
    }

    @Override
    public void setRole(final Role role) {
        setRequiredProperty(SemanticSynchrony.PropertyKeys.ROLE, role.name());
    }

    @Override
    public Topic getTarget() {
        return getExactlyOneEntity(SemanticSynchrony.EdgeLabels.TARGET, Direction.OUT,
                vertex -> getGraph().asTopic(vertex));
    }

    @Override
    public void setTarget(Topic target) {
        setRequiredEntity(SemanticSynchrony.EdgeLabels.TARGET, target);
    }

    @Override
    public String getLabel() {
        return getRequiredProperty(SemanticSynchrony.PropertyKeys.LABEL);
    }

    @Override
    public void setLabel(String label) {
        setRequiredProperty(SemanticSynchrony.PropertyKeys.LABEL, label);
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
