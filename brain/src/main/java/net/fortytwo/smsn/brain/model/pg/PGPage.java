package net.fortytwo.smsn.brain.model.pg;

import net.fortytwo.smsn.SemanticSynchrony;
import net.fortytwo.smsn.brain.model.entities.EntityList;
import net.fortytwo.smsn.brain.model.entities.KeyValueTree;
import net.fortytwo.smsn.brain.model.entities.Link;
import net.fortytwo.smsn.brain.model.entities.Page;
import net.fortytwo.smsn.brain.model.entities.Topic;
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
    public boolean setAlias(String alias) {
        return setOptionalProperty(SemanticSynchrony.PropertyKeys.ALIAS, alias);
    }

    @Override
    public Long getCreated() {
        return getOptionalProperty(SemanticSynchrony.PropertyKeys.CREATED);
    }

    @Override
    public boolean setCreated(Long created) {
        return setRequiredProperty(SemanticSynchrony.PropertyKeys.CREATED, created);
    }

    @Override
    public String getFormat() {
        return getRequiredProperty(SemanticSynchrony.PropertyKeys.FORMAT);
    }

    @Override
    public boolean setFormat(String format) {
        return setRequiredProperty(SemanticSynchrony.PropertyKeys.FORMAT, format);
    }

    @Override
    public String getText() {
        return (String) getOptionalProperty(SemanticSynchrony.PropertyKeys.TEXT);
    }

    @Override
    public boolean setText(String text) {
        return setOptionalProperty(SemanticSynchrony.PropertyKeys.TEXT, text);
    }

    @Override
    public Float getPriority() {
        return getOptionalProperty(SemanticSynchrony.PropertyKeys.PRIORITY);
    }

    @Override
    public boolean setPriority(Float priority) {
        return setOptionalProperty(SemanticSynchrony.PropertyKeys.PRIORITY, priority);
    }

    @Override
    public String getSource() {
        return getRequiredProperty(SemanticSynchrony.PropertyKeys.SOURCE);
    }

    @Override
    public boolean setSource(String source) {
        return setRequiredProperty(SemanticSynchrony.PropertyKeys.SOURCE, source);
    }

    @Override
    public String getShortcut() {
        return getOptionalProperty(SemanticSynchrony.PropertyKeys.SHORTCUT);
    }

    @Override
    public boolean setShortcut(String shortcut) {
        return setOptionalProperty(SemanticSynchrony.PropertyKeys.SHORTCUT, shortcut);
    }

    @Override
    public Float getWeight() {
        return getOptionalProperty(SemanticSynchrony.PropertyKeys.WEIGHT, 0f);
    }

    @Override
    public boolean setWeight(Float weight) {
        return setRequiredProperty(SemanticSynchrony.PropertyKeys.WEIGHT, weight);
    }

    @Override
    public Topic getPrimaryTopic() {
        return getExactlyOneEntity(SemanticSynchrony.EdgeLabels.TOPIC, Direction.OUT, v -> getGraph().asTopic(v));
    }

    @Override
    public boolean setPrimaryTopic(Topic topic) {
        return setRequiredEntity(SemanticSynchrony.EdgeLabels.TOPIC, topic);
    }

    @Override
    public KeyValueTree<Link, EntityList<Link>> getContent() {
        return getExactlyOneEntity(SemanticSynchrony.EdgeLabels.CONTENT, Direction.OUT, v -> getGraph().asLinkTree(v));
    }

    @Override
    public boolean setContent(KeyValueTree<Link, EntityList<Link>> tree) {
        return setRequiredEntity(SemanticSynchrony.EdgeLabels.CONTENT, tree);
    }

    @Override
    public void destroy() {
        // a page does not own its primary topic, but it does own its topic tree
        getContent().destroy();

        destroyInternal();
    }
}
