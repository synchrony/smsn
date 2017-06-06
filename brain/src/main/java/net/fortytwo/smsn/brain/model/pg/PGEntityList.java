package net.fortytwo.smsn.brain.model.pg;

import net.fortytwo.smsn.SemanticSynchrony;
import net.fortytwo.smsn.brain.model.entities.Entity;
import net.fortytwo.smsn.brain.model.entities.EntityList;
import org.apache.tinkerpop.gremlin.structure.Direction;
import org.apache.tinkerpop.gremlin.structure.Vertex;

import java.util.function.Function;

public abstract class PGEntityList<T extends Entity> extends PGEntity implements EntityList<T> {

    private final Function<Vertex, T> constructor;

    public PGEntityList(final Vertex vertex,
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
    public EntityList<T> getRest() {
        return getAtMostOneEntity(SemanticSynchrony.EdgeLabels.REST, Direction.OUT,
                vertex -> getGraph().asEntityList(vertex, constructor));
    }

    @Override
    public boolean setRest(EntityList<T> rest) {
        return setOptionalEntity(SemanticSynchrony.EdgeLabels.REST, rest);
    }

    @Override
    public EntityList<T> getRestOf() {
        return getAtMostOneEntity(SemanticSynchrony.EdgeLabels.REST, Direction.IN,
                vertex -> getGraph().asEntityList(vertex, constructor));
    }

    @Override
    public void destroy() {
        // a list owns each list element
        getFirst().destroy();

        // the head of a list has exclusive ownership of the tail
        EntityList<T> rest = getRest();
        if (null != rest) {
            rest.destroy();
        }

        destroyInternal();
    }
}
