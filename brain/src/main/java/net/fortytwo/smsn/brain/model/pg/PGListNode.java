package net.fortytwo.smsn.brain.model.pg;

import net.fortytwo.smsn.SemanticSynchrony;
import net.fortytwo.smsn.brain.model.entities.Node;
import net.fortytwo.smsn.brain.model.entities.ListNode;
import org.apache.tinkerpop.gremlin.structure.Direction;
import org.apache.tinkerpop.gremlin.structure.Vertex;

import java.util.function.Function;

public abstract class PGListNode<T extends Node> extends PGEntity implements ListNode<T> {

    private final Function<Vertex, T> constructor;

    public PGListNode(final Vertex vertex,
                      final Function<Vertex, T> constructor) {
        super(vertex);
        this.constructor = constructor;
    }

    @Override
    public T getFirst() {
        return getExactlyOneEntity(SemanticSynchrony.EdgeLabels.FIRST, Direction.OUT, constructor);
    }

    @Override
    public boolean setFirst(T first) {
        return setRequiredEntity(SemanticSynchrony.EdgeLabels.FIRST, first);
    }

    @Override
    public ListNode<T> getRest() {
        return getAtMostOneEntity(SemanticSynchrony.EdgeLabels.REST, Direction.OUT,
                vertex -> getGraph().asEntityList(vertex, constructor));
    }

    @Override
    public boolean setRest(ListNode<T> rest) {
        return setOptionalEntity(SemanticSynchrony.EdgeLabels.REST, rest);
    }

    @Override
    public ListNode<T> getRestOf() {
        return getAtMostOneEntity(SemanticSynchrony.EdgeLabels.REST, Direction.IN,
                vertex -> getGraph().asEntityList(vertex, constructor));
    }

    @Override
    public int length() {
        return null == getRest() ? 1 : 1 + getRest().length();
    }

    @Override
    public T get(final int index) {
        return 0 == index ? getFirst() : getRest().get(index - 1);
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

        destroyInternal();
    }
}
