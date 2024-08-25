package net.fortytwo.smsn.brain.model.pg;

import net.fortytwo.smsn.SemanticSynchrony;
import net.fortytwo.smsn.brain.model.Role;
import net.fortytwo.smsn.brain.model.entities.TreeNode;
import net.fortytwo.smsn.brain.model.entities.Link;
import net.fortytwo.smsn.brain.model.entities.Page;
import org.apache.tinkerpop.gremlin.structure.Direction;
import org.apache.tinkerpop.gremlin.structure.Vertex;

public abstract class PGPage implements PGEntity, Page {

    private final Vertex vertex;

    @Override
    public Vertex asVertex() {
        return vertex;
    }

    @Override
    public boolean equals(Object other) {
        return other instanceof PGPage && PGEntity.equals(asVertex(), ((PGPage) other).asVertex());
    }
    @Override
    public int hashCode() {
        return PGEntity.hashCode(asVertex());
    }

    public PGPage(Vertex vertex) {
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
    public String getAlias() {
        return PGEntity.getOptionalProperty(asVertex(), SemanticSynchrony.PropertyKeys.ALIAS);
    }

    @Override
    public void setAlias(String alias) {
        PGEntity.setOptionalProperty(asVertex(), SemanticSynchrony.PropertyKeys.ALIAS, alias);
    }

    @Override
    public String getText() {
        return (String) PGEntity.getOptionalProperty(asVertex(), SemanticSynchrony.PropertyKeys.TEXT);
    }

    @Override
    public void setText(String text) {
        PGEntity.setOptionalProperty(asVertex(), SemanticSynchrony.PropertyKeys.TEXT, text);
    }

    @Override
    public Float getPriority() {
        return PGEntity.getOptionalProperty(asVertex(), SemanticSynchrony.PropertyKeys.PRIORITY);
    }

    @Override
    public void setPriority(Float priority) {
        PGEntity.setOptionalProperty(asVertex(), SemanticSynchrony.PropertyKeys.PRIORITY, priority);
    }

    @Override
    public Long getCreated() {
        return PGEntity.getRequiredProperty(asVertex(), SemanticSynchrony.PropertyKeys.CREATED);
    }

    @Override
    public void setCreated(Long created) {
        PGEntity.setRequiredEntity(asVertex(), SemanticSynchrony.PropertyKeys.CREATED, created);
    }

    @Override
    public String getSource() {
        return PGEntity.getRequiredProperty(asVertex(), SemanticSynchrony.PropertyKeys.SOURCE);
    }

    @Override
    public void setSource(String source) {
        PGEntity.setRequiredProperty(asVertex(), SemanticSynchrony.PropertyKeys.SOURCE, source);
    }

    @Override
    public String getShortcut() {
        return PGEntity.getOptionalProperty(asVertex(), SemanticSynchrony.PropertyKeys.SHORTCUT);
    }

    @Override
    public void setShortcut(String shortcut) {
        PGEntity.setOptionalProperty(asVertex(), SemanticSynchrony.PropertyKeys.SHORTCUT, shortcut);
    }

    @Override
    public Float getWeight() {
        return PGEntity.getOptionalProperty(asVertex(), SemanticSynchrony.PropertyKeys.WEIGHT, 0f);
    }

    @Override
    public void setWeight(Float weight) {
        PGEntity.setRequiredProperty(asVertex(), SemanticSynchrony.PropertyKeys.WEIGHT, weight);
    }

    @Override
    public TreeNode<Link> getContent() {
        return PGEntity.getExactlyOneEntity(asVertex(), SemanticSynchrony.EdgeLabels.CONTENT, Direction.OUT, v -> getGraph().asLinkTree(v));
    }

    @Override
    public void setContent(TreeNode<Link> tree) {
        PGEntity.setRequiredEntity(asVertex(), SemanticSynchrony.EdgeLabels.CONTENT, tree);
    }

    @Override
    public void destroy() {
        // a page does not own its primary topic, but it does own its topic tree
        getContent().destroy();

        PGEntity.destroyInternal(asVertex());
    }
}
