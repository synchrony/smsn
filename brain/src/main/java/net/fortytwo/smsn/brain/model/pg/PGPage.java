package net.fortytwo.smsn.brain.model.pg;

import net.fortytwo.smsn.SemanticSynchrony;
import net.fortytwo.smsn.brain.model.entities.TreeNode;
import net.fortytwo.smsn.brain.model.entities.Link;
import net.fortytwo.smsn.brain.model.entities.Page;
import org.apache.tinkerpop.gremlin.structure.Direction;
import org.apache.tinkerpop.gremlin.structure.Vertex;

public abstract class PGPage extends PGEntity implements Page {

    public PGPage(Vertex vertex) {
        super(vertex);
    }

    @Override
    public String getAlias() {
        return getOptionalProperty(SemanticSynchrony.PropertyKeys.ALIAS);
    }

    @Override
    public void setAlias(String alias) {
        setOptionalProperty(SemanticSynchrony.PropertyKeys.ALIAS, alias);
    }

    @Override
    public String getText() {
        return (String) getOptionalProperty(SemanticSynchrony.PropertyKeys.TEXT);
    }

    @Override
    public void setText(String text) {
        setOptionalProperty(SemanticSynchrony.PropertyKeys.TEXT, text);
    }

    @Override
    public Float getPriority() {
        return getOptionalProperty(SemanticSynchrony.PropertyKeys.PRIORITY);
    }

    @Override
    public void setPriority(Float priority) {
        setOptionalProperty(SemanticSynchrony.PropertyKeys.PRIORITY, priority);
    }

    @Override
    public String getSource() {
        return getRequiredProperty(SemanticSynchrony.PropertyKeys.SOURCE);
    }

    @Override
    public void setSource(String source) {
        setRequiredProperty(SemanticSynchrony.PropertyKeys.SOURCE, source);
    }

    @Override
    public String getShortcut() {
        return getOptionalProperty(SemanticSynchrony.PropertyKeys.SHORTCUT);
    }

    @Override
    public void setShortcut(String shortcut) {
        setOptionalProperty(SemanticSynchrony.PropertyKeys.SHORTCUT, shortcut);
    }

    @Override
    public Float getWeight() {
        return getOptionalProperty(SemanticSynchrony.PropertyKeys.WEIGHT, 0f);
    }

    @Override
    public void setWeight(Float weight) {
        setRequiredProperty(SemanticSynchrony.PropertyKeys.WEIGHT, weight);
    }

    @Override
    public TreeNode<Link> getContent() {
        return getExactlyOneEntity(SemanticSynchrony.EdgeLabels.CONTENT, Direction.OUT, v -> getGraph().asLinkTree(v));
    }

    @Override
    public void setContent(TreeNode<Link> tree) {
        setRequiredEntity(SemanticSynchrony.EdgeLabels.CONTENT, tree);
    }

    @Override
    public void destroy() {
        // a page does not own its primary topic, but it does own its topic tree
        getContent().destroy();

        destroyInternal();
    }
}
