package net.fortytwo.smsn.brain.model.pg;

import net.fortytwo.smsn.SemanticSynchrony;
import net.fortytwo.smsn.brain.model.entities.ListNode;
import net.fortytwo.smsn.brain.model.entities.Entity;
import org.apache.tinkerpop.gremlin.structure.Direction;
import org.apache.tinkerpop.gremlin.structure.Vertex;

import java.util.function.Function;

public abstract class PGListNode<T extends Entity> implements PGEntity, ListNode<T> {

    private final Vertex vertex;

    @Override
    public Vertex asVertex() {
        return vertex;
    }

    @Override
    public boolean equals(Object other) {
        return other instanceof PGListNode && PGEntity.equals(asVertex(), ((PGListNode) other).asVertex());
    }
    @Override
    public int hashCode() {
        return PGEntity.hashCode(asVertex());
    }

    private final Function<Vertex, T> constructor;

    public PGListNode(final Vertex vertex,
                      final Function<Vertex, T> constructor) {
        this.vertex = vertex;
        this.constructor = constructor;
    }

    @Override
    public T getFirst() {
        return PGEntity.getExactlyOneEntity(asVertex(), SemanticSynchrony.EdgeLabels.FIRST, Direction.OUT, constructor);
    }

    @Override
    public void setFirst(T first) {
        PGEntity.setRequiredEntity(asVertex(), SemanticSynchrony.EdgeLabels.FIRST, first);
    }

    @Override
    public ListNode<T> getRest() {
        return PGEntity.getAtMostOneEntity(asVertex(), SemanticSynchrony.EdgeLabels.REST, Direction.OUT,
                vertex -> getGraph().asEntityList(vertex, constructor));
    }

    @Override
    public void setRest(ListNode<T> rest) {
        PGEntity.setOptionalEntity(asVertex(), SemanticSynchrony.EdgeLabels.REST, rest);
    }

    @Override
    public ListNode<T> getRestOf() {
        return PGEntity.getAtMostOneEntity(asVertex(), SemanticSynchrony.EdgeLabels.REST, Direction.IN,
                vertex -> getGraph().asEntityList(vertex, constructor));
    }

    @Override
    public int length() {
        return null == getRest() ? 1 : 1 + getRest().length();
    }

    @Override
    public T get(final int index) {
        return ListNode.get(this, index);
    }

    @Override
    public ListNode<T> add(int index, T toAdd) {
        return ListNode.add(this, index, toAdd,
                (t, tListNode) -> getGraph().createListNode(t, tListNode, constructor));
    }

    @Override
    public ListNode<T> remove(int index) {
        return ListNode.remove(this, index);
    }

    @Override
    public void destroy() {
        // a list owns each list element
        getFirst().destroy();

        // the head of a list has exclusive ownership of the tail
        ListNode<T> rest = getRest();
        if (null != rest) {
            rest.destroy();
        }

        PGEntity.destroyInternal(asVertex());
    }
}
