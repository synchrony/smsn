package net.fortytwo.smsn.brain.model.pg;

import net.fortytwo.smsn.SemanticSynchrony;
import net.fortytwo.smsn.brain.model.entities.Node;
import net.fortytwo.smsn.brain.model.entities.ListNode;
import net.fortytwo.smsn.brain.model.entities.TreeNode;
import org.apache.tinkerpop.gremlin.structure.Direction;
import org.apache.tinkerpop.gremlin.structure.Vertex;

import java.util.function.Function;

public abstract class PGTreeNode<T extends Node> extends PGEntity implements TreeNode<T> {

    private final Function<Vertex, T> constructor;

    public PGTreeNode(Vertex vertex,
                      Function<Vertex, T> constructor) {
        super(vertex);
        this.constructor = constructor;
    }

    @Override
    public T getValue() {
        return getAtMostOneEntity(SemanticSynchrony.EdgeLabels.VALUE, Direction.OUT, constructor);
    }

    @Override
    public void setValue(T value) {
        setRequiredEntity(SemanticSynchrony.EdgeLabels.VALUE, value);
    }

    @Override
    public ListNode<TreeNode<T>> getChildren() {
        return getAtMostOneEntity(SemanticSynchrony.EdgeLabels.CHILDREN, Direction.OUT,
                v -> getGraph().asEntityList(v,
                        vertex -> getGraph().asEntityTree(vertex, constructor)));
    }

    @Override
    public void setChildren(ListNode<TreeNode<T>> children) {
        setOptionalEntity(SemanticSynchrony.EdgeLabels.CHILDREN, children);
    }

    @Override
    public void destroy() {
        // a tree owns its value
        T value = getValue();
        if (null != value) {
            value.destroy();
        }

        // a tree owns its children
        ListNode<TreeNode<T>> children = getChildren();
        if (null != children) {
            children.destroy();
        }
    }
}
