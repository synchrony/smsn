package net.fortytwo.smsn.brain.model.pg;

import com.google.common.base.Preconditions;
import net.fortytwo.smsn.brain.error.InvalidGraphException;
import org.apache.tinkerpop.gremlin.structure.Direction;
import org.apache.tinkerpop.gremlin.structure.Edge;
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

    private <T> boolean setProperty(String name, T value) {
        Object previousValue = vertex.property(name);

        if (null == value) {
            if (null == previousValue) {
                return false;
            } else {
                vertex.property(name).remove();
                return true;
            }
        } else {
            if (null == previousValue || !value.equals(previousValue)) {
                vertex.property(name, value);
                return true;
            } else {
                return false;
            }
        }
    }

    protected boolean setOptionalProperty(String name, Object value) {
        return setProperty(name, value);
    }

    protected boolean setRequiredProperty(String name, Object value) {
        if (null == value) {
            throw new IllegalArgumentException("can't clear required property '" + name
                    + "' on " + this);
        }

        return setProperty(name, value);
    }

    protected boolean setRequiredEntity(final String label, final Object other) {
        return setEntity(label, other, true);
    }

    protected boolean setOptionalEntity(final String label, final Object other) {
        return setEntity(label, other, false);
    }

    private boolean setEntity(final String label, final Object other, final boolean required) {
        Preconditions.checkArgument(!required || null != other);

        boolean changed = removeEdge(label, Direction.OUT);
        if (null != other) {
            addOutEdge(((PGEntity) other).asVertex(), label);
            changed = true;
        }
        return changed;
    }

    protected void forAllVertices(final String label, final Direction direction, final Consumer<Vertex> consumer) {
        vertex.vertices(direction, label).forEachRemaining(consumer);
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

    protected void forEachAdjacentVertex(final String label, Direction direction, Consumer<Vertex> consumer) {
        vertex.vertices(direction, label).forEachRemaining(consumer);
    }

    protected boolean hasAdjacentVertex(final String label, Direction direction) {
        return vertex.vertices(direction, label).hasNext();
    }

    protected boolean removeEdge(final String label, Direction direction) {
        final Mutable<Boolean> changed = new Mutable<>(false);
        asVertex().edges(direction, label).forEachRemaining(
                edge -> {
                    edge.remove();
                    changed.value = true;
                });
        return changed.value;
    }

    protected void destroyInternal() {
        vertex.remove();
    }

    private static class Mutable<T> {
        public T value;

        public Mutable(T value) {
            this.value = value;
        }
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

    @Override
    public String toString() {
        String className = getClass().getSimpleName();
        if (0 == className.length()) {
            className = "vertex";
        }
        return className + "[" + vertex.id() + "]";
    }
}
