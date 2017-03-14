package net.fortytwo.smsn.brain.model.pg;

import net.fortytwo.smsn.SemanticSynchrony;
import net.fortytwo.smsn.brain.model.entities.Entity;
import net.fortytwo.smsn.brain.model.entities.EntityList;
import net.fortytwo.smsn.brain.model.entities.KeyValueTree;
import org.apache.tinkerpop.gremlin.structure.Direction;
import org.apache.tinkerpop.gremlin.structure.Vertex;

import java.util.function.Function;

public abstract class PGKeyValueTree<K extends Entity, V extends Entity>
        extends PGEntity implements KeyValueTree<K, V> {

    private final Function<Vertex, K> keyConstructor;
    private final Function<Vertex, V> valueConstructor;

    public PGKeyValueTree(Vertex vertex,
                          Function<Vertex, K> keyConstructor,
                          Function<Vertex, V> valueConstructor) {
        super(vertex);
        this.keyConstructor = keyConstructor;
        this.valueConstructor = valueConstructor;
    }

    @Override
    public K getKey() {
        return getExactlyOneEntity(SemanticSynchrony.EdgeLabels.KEY, Direction.OUT, keyConstructor);
    }

    @Override
    public boolean setKey(K key) {
        return setRequiredEntity(SemanticSynchrony.EdgeLabels.KEY, key);
    }

    @Override
    public V getValue() {
        return getAtMostOneEntity(SemanticSynchrony.EdgeLabels.VALUE, Direction.OUT, valueConstructor);
    }

    @Override
    public boolean setValue(V value) {
        return setOptionalEntity(SemanticSynchrony.EdgeLabels.VALUE, value);
    }

    @Override
    public EntityList<KeyValueTree<K, V>> getChildren() {
        return getAtMostOneEntity(SemanticSynchrony.EdgeLabels.CHILDREN, Direction.OUT,
                v -> getGraph().asEntityList(v,
                        vertex -> getGraph().asEntityTree(vertex, keyConstructor, valueConstructor)));
    }

    @Override
    public boolean setChildren(EntityList<KeyValueTree<K, V>> children) {
        return setOptionalEntity(SemanticSynchrony.EdgeLabels.CHILDREN, children);
    }

    @Override
    public void destroy() {
        // a tree owns its key
        getKey().destroy();

        // a tree owns its value
        V value = getValue();
        if (null != value) {
            value.destroy();
        }

        // a tree owns its children
        EntityList<KeyValueTree<K, V>> children = getChildren();
        if (null != children) {
            children.destroy();
        }
    }
}
