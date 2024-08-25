package net.fortytwo.smsn.brain.model.pg;

import net.fortytwo.smsn.SemanticSynchrony;
import net.fortytwo.smsn.brain.model.entities.Entity;
import net.fortytwo.smsn.brain.model.entities.ListNode;
import net.fortytwo.smsn.brain.model.entities.TreeNode;
import org.apache.tinkerpop.gremlin.structure.Direction;
import org.apache.tinkerpop.gremlin.structure.Vertex;

import java.util.function.Function;

public abstract class PGTreeNode<T extends Entity> implements PGEntity, TreeNode<T> {

    private final Vertex vertex;

    @Override
    public Vertex asVertex() {
        return vertex;
    }

    @Override
    public boolean equals(Object other) {
        return other instanceof PGTreeNode && PGEntity.equals(asVertex(), ((PGTreeNode) other).asVertex());
    }
    @Override
    public int hashCode() {
        return PGEntity.hashCode(asVertex());
    }

    private final Function<Vertex, T> constructor;

    public PGTreeNode(Vertex vertex,
                      Function<Vertex, T> constructor) {
        this.vertex = vertex;
        this.constructor = constructor;
    }

    @Override
    public T getValue() {
        return PGEntity.getAtMostOneEntity(asVertex(), SemanticSynchrony.EdgeLabels.VALUE, Direction.OUT, constructor);
    }

    @Override
    public void setValue(T value) {
        PGEntity.setRequiredEntity(asVertex(), SemanticSynchrony.EdgeLabels.VALUE, value);
    }

    @Override
    public ListNode<TreeNode<T>> getChildren() {
        return PGEntity.getAtMostOneEntity(asVertex(), SemanticSynchrony.EdgeLabels.CHILDREN, Direction.OUT,
                v -> getGraph().asEntityList(v,
                        vertex -> getGraph().asEntityTree(vertex, constructor)));
    }

    @Override
    public void setChildren(ListNode<TreeNode<T>> children) {
        PGEntity.setOptionalEntity(asVertex(), SemanticSynchrony.EdgeLabels.CHILDREN, children);
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

    // TODO: use of addChild is currently inefficient.  Use stack rather than queue semantics.
    @Override
    public void addChild(TreeNode<T> child) {
        appendChild(this, child);
    }

    @Override
    public int getNumberOfChildren() {
        throw new UnsupportedOperationException();
    }

    @Override
    public void setNumberOfChildren(int numberOfChildren) {
        throw new UnsupportedOperationException();
    }

    @Override
    public int getNumberOfParents() {
        throw new UnsupportedOperationException();
    }

    @Override
    public void setNumberOfParents(int numberOfParents) {
        throw new UnsupportedOperationException();
    }

    private void appendChild(TreeNode<T> tree, TreeNode<T> child) {
        ListNode<TreeNode<T>> toInsert = getGraph().createListNode(child, null,
                vertex -> getGraph().asEntityTree(vertex, constructor));

        ListNode<TreeNode<T>> cur = tree.getChildren();
        ListNode<TreeNode<T>> prev = null;
        while (null != cur) {
            prev = cur;
            cur = cur.getRest();
        }

        if (null == prev) {
            tree.setChildren(toInsert);
        } else {
            prev.setRest(toInsert);
        }
    }
}
