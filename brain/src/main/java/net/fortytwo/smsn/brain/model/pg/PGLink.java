package net.fortytwo.smsn.brain.model.pg;

import net.fortytwo.smsn.SemanticSynchrony;
import net.fortytwo.smsn.brain.model.Tag;
import net.fortytwo.smsn.brain.model.entities.Link;
import net.fortytwo.smsn.brain.model.entities.Page;
import net.fortytwo.smsn.brain.model.entities.Topic;
import org.apache.tinkerpop.gremlin.structure.Direction;
import org.apache.tinkerpop.gremlin.structure.Vertex;

public abstract class PGLink extends PGEntity implements Link {

    protected PGLink(Vertex vertex) {
        super(vertex);
    }

    public Tag getTag() {
        String name = getOptionalProperty(SemanticSynchrony.PropertyKeys.TAG);
        return null == name ? null : Tag.valueOf(name);
    }

    public void setTag(final Tag tag) {
        setOptionalProperty(SemanticSynchrony.PropertyKeys.TAG, null == tag ? null : tag.name());
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
    public Page getPage() {
        return getExactlyOneEntity(SemanticSynchrony.EdgeLabels.PAGE, Direction.OUT,
                vertex -> getGraph().asPage(vertex));
    }

    @Override
    public void setPage(Page page) {
        setRequiredEntity(SemanticSynchrony.EdgeLabels.PAGE, page);
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
