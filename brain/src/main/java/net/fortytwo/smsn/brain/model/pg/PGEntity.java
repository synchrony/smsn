package net.fortytwo.smsn.brain.model.pg;

import com.google.common.base.Preconditions;
import net.fortytwo.smsn.brain.error.InvalidGraphException;
import net.fortytwo.smsn.brain.model.entities.Entity;
import org.apache.tinkerpop.gremlin.structure.Direction;
import org.apache.tinkerpop.gremlin.structure.Edge;
import org.apache.tinkerpop.gremlin.structure.Element;
import org.apache.tinkerpop.gremlin.structure.Vertex;
import org.apache.tinkerpop.gremlin.structure.VertexProperty;

import java.util.Iterator;
import java.util.function.Consumer;
import java.util.function.Function;

abstract class PGEntity {

    private final Vertex vertex;

    protected PGEntity(final Vertex vertex) {
        this.vertex = vertex;
    }

    @Override
    public boolean equals(Object other) {
        return other instanceof PGEntity && ((PGEntity) other).vertex.id().equals(vertex.id());
    }

    @Override
    public int hashCode() {
        return vertex.id().hashCode();
    }

    public void remove() {
        vertex.remove();
    }

    protected abstract PGTopicGraph getGraph();

    public Vertex asVertex() {
        return vertex;
    }

    protected void addOutEdge(final Vertex inVertex, final String label) {
        asVertex().addEdge(label, inVertex);
    }

    protected <T> T getOptionalProperty(String name) {
        VertexProperty<T> property = vertex.property(name);
        return property.isPresent() ? property.value() : null;
    }

    protected <T> T getOptionalProperty(String name, T defaultValue) {
        T value = getOptionalProperty(name);
        return null == value ? defaultValue : value;
    }

    protected <T> T getRequiredProperty(String name) {
        T value = getOptionalProperty(name);
        if (null == value) {
            throw new InvalidGraphException("missing property '" + name + "' for " + this);
        }
        return value;
    }

    private <T> void setProperty(String name, T value) {
        Object previousValue = vertex.property(name);

        if (null == value) {
            if (null != previousValue) {
                vertex.property(name).remove();
            }
        } else {
            if (null == previousValue || !value.equals(previousValue)) {
                vertex.property(name, value);
            }
        }
    }

    protected void setOptionalProperty(String name, Object value) {
        setProperty(name, value);
    }

    protected void setRequiredProperty(String name, Object value) {
        if (null == value) {
            throw new IllegalArgumentException("can't clear required property '" + name
                    + "' on " + this);
        }

        setProperty(name, value);
    }

    protected void setRequiredEntity(final String label, final Entity other) {
        setEntity(label, other, true);
    }

    protected void setOptionalEntity(final String label, final Entity other) {
        setEntity(label, other, false);
    }

    private void setEntity(final String label, final Entity other, final boolean required) {
        Preconditions.checkArgument(!required || null != other);

        if (null == other) {
            removeEdges(label, Direction.OUT);
        } else {
            // note: uniqueness of edges is not guaranteed
            addOutEdge(((PGEntity) other).asVertex(), label);
        }
    }

    private Vertex getAtMostOneVertex(final String label, final Direction direction) {
        Edge edge = getAtMostOneEdge(label, direction);
        return null == edge ? null : getVertex(edge, direction.opposite());
    }

    private Vertex getExactlyOneVertex(final String label, final Direction direction) {
        return getVertex(getExactlyOneEdge(label, direction), direction.opposite());
    }

    protected <T> T getExactlyOneEntity(
            final String label, final Direction direction, final Function<Vertex, T> constructor) {
        return constructor.apply(getExactlyOneVertex(label, direction));
    }

    protected <T> T getAtMostOneEntity(
            final String label, final Direction direction, final Function<Vertex, T> constructor) {
        Vertex vertex = getAtMostOneVertex(label, direction);
        return null == vertex ? null : constructor.apply(vertex);
    }

    protected <T> Iterator<T> getEntities(
            final String label, final Direction direction, final Function<Vertex, T> constructor) {
        return mapIterator(getVertices(asVertex(), label, direction), constructor);
    }

    private Edge getAtMostOneEdge(final String label, final Direction direction) {
        Iterator<Edge> iter = vertex.edges(direction, label);
        if (!iter.hasNext()) {
            return null;
        }
        Edge result = iter.next();
        if (iter.hasNext()) {
            throw new InvalidGraphException("vertex " + this
                    + " has more than one '" + label + "' " + direction + " edge");
        }
        return result;
    }

    private Edge getExactlyOneEdge(final String label, final Direction direction) {
        Edge other = getAtMostOneEdge(label, direction);
        if (null == other) {
            throw new InvalidGraphException("vertex " + this
                    + " is missing '" + label + "' " + direction + " edge");
        }
        return other;
    }

    protected void forEachAdjacentVertex(final String label, Direction direction, Consumer<Vertex> visitor) {
        vertex.vertices(direction, label).forEachRemaining(visitor);
    }

    protected boolean hasAdjacentVertex(final String label, Direction direction) {
        return vertex.vertices(direction, label).hasNext();
    }

    protected void removeEdges(final String label, Direction direction) {
        asVertex().edges(direction, label).forEachRemaining(Element::remove);
    }

    protected void destroyInternal() {
        vertex.remove();
    }

    private Iterator<Vertex> getVertices(final Vertex start, final String label, final Direction direction) {
        return start.vertices(direction, label);
    }

    private Vertex getVertex(final Edge edge, final Direction direction) {
        switch (direction) {
            case OUT:
                return edge.outVertex();
            case IN:
                return edge.inVertex();
            default:
                throw new IllegalStateException();
        }
    }

    private <D, R> Iterator<R> mapIterator(final Iterator<D> base, final Function<D, R> mapping) {
        return new Iterator<R>() {
            @Override
            public boolean hasNext() {
                return base.hasNext();
            }

            @Override
            public R next() {
                return mapping.apply(base.next());
            }
        };
    }

    @Override
    public String toString() {
        String className = getClass().getSimpleName();
        if (0 == className.length()) {
            className = "vertex";
        }
        return className + "[" + vertex.id() + "]";
    }
}
