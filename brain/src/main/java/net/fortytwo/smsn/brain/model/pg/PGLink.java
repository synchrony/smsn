package net.fortytwo.smsn.brain.model.pg;

import net.fortytwo.smsn.SemanticSynchrony;
import net.fortytwo.smsn.brain.model.Role;
import net.fortytwo.smsn.brain.model.entities.Link;
import net.fortytwo.smsn.brain.model.entities.Page;
import net.fortytwo.smsn.brain.model.entities.Topic;
import org.apache.tinkerpop.gremlin.structure.Direction;
import org.apache.tinkerpop.gremlin.structure.Vertex;

public abstract class PGLink implements PGEntity, Link {

    private final Vertex vertex;

    @Override
    public Vertex asVertex() {
        return vertex;
    }

    @Override
    public boolean equals(Object other) {
        return other instanceof PGLink && PGEntity.equals(asVertex(), ((PGLink) other).asVertex());
    }
    @Override
    public int hashCode() {
        return PGEntity.hashCode(asVertex());
    }

    protected PGLink(Vertex vertex) {
        this.vertex = vertex;
    }

    public Role getRole() {
        String name = PGEntity.getOptionalProperty(asVertex(), SemanticSynchrony.PropertyKeys.ROLE);
        return null == name ? null : Role.valueOf(name);
    }

    public void setRole(final Role role) {
        PGEntity.setOptionalProperty(asVertex(), SemanticSynchrony.PropertyKeys.ROLE, null == role ? null : role.name());
    }

    @Override
    public Topic getTarget() {
        return PGEntity.getExactlyOneEntity(asVertex(), SemanticSynchrony.EdgeLabels.TARGET, Direction.OUT,
                vertex -> getGraph().asTopic(vertex));
    }

    @Override
    public void setTarget(Topic target) {
        PGEntity.setRequiredEntity(asVertex(), SemanticSynchrony.EdgeLabels.TARGET, target);
    }

    @Override
    public String getLabel() {
        return PGEntity.getRequiredProperty(asVertex(), SemanticSynchrony.PropertyKeys.LABEL);
    }

    @Override
    public void setLabel(String label) {
        PGEntity.setRequiredProperty(asVertex(), SemanticSynchrony.PropertyKeys.LABEL, label);
    }

    @Override
    public Page getPage() {
        return PGEntity.getExactlyOneEntity(asVertex(), SemanticSynchrony.EdgeLabels.PAGE, Direction.OUT,
                vertex -> getGraph().asPage(vertex));
    }

    @Override
    public void setPage(Page page) {
        PGEntity.setRequiredEntity(asVertex(), SemanticSynchrony.EdgeLabels.PAGE, page);
    }

    @Override
    public void destroy() {
        Topic target = getTarget();

        PGEntity.destroyInternal(asVertex());

        // a link owns its target only if there are no other links to the target
        if (target.isIsolated()) {
            target.destroy();
        }
    }
}
