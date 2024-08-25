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

interface PGEntity {

    Vertex asVertex();

    static boolean equals(final Vertex vertex, Object other) {
        return other instanceof Vertex && ((Vertex) other).id().equals(vertex.id());
    }

    static int hashCode(final Vertex vertex) {
        return vertex.id().hashCode();
    }

    PGTopicGraph getGraph();

    static void addOutEdge(final Vertex vertex, final Vertex inVertex, final String label) {
        vertex.addEdge(label, inVertex);
    }

    static <T> T getOptionalProperty(final Vertex vertex, String name) {
        VertexProperty<T> property = vertex.property(name);
        return property.isPresent() ? property.value() : null;
    }

    static <T> T getOptionalProperty(final Vertex vertex, String name, T defaultValue) {
        T value = getOptionalProperty(vertex, name);
        return null == value ? defaultValue : value;
    }

    static <T> T getRequiredProperty(final Vertex vertex, String name) {
        T value = getOptionalProperty(vertex, name);
        if (null == value) {
            throw new InvalidGraphException("missing property '" + name + "' for " + toString(vertex));
        }
        return value;
    }

    private static <T> boolean setProperty(final Vertex vertex, String name, T value) {
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

    static boolean setOptionalProperty(final Vertex vertex, String name, Object value) {
        return setProperty(vertex, name, value);
    }

    static boolean setRequiredProperty(final Vertex vertex, String name, Object value) {
        if (null == value) {
            throw new IllegalArgumentException("can't clear required property '" + name
                    + "' on " + toString(vertex));
        }

        return setProperty(vertex, name, value);
    }

    static boolean setRequiredEntity(final Vertex vertex, final String label, final Object other) {
        return setEntity(vertex, label, other, true);
    }

    static boolean setOptionalEntity(final Vertex vertex, final String label, final Object other) {
        return setEntity(vertex, label, other, false);
    }

    private static boolean setEntity(Vertex vertex, final String label, final Object other, final boolean required) {
        Preconditions.checkArgument(!required || null != other);

        boolean changed = PGEntity.removeEdge(vertex, label, Direction.OUT);
        if (null != other) {
            addOutEdge(vertex, ((PGEntity) other).asVertex(), label);
            changed = true;
        }
        return changed;
    }

    static void forAllVertices(final Vertex vertex,
                                         final String label, final Direction direction, final Consumer<Vertex> consumer) {
        vertex.vertices(direction, label).forEachRemaining(consumer);
    }

    private static Vertex getAtMostOneVertex(final Vertex vertex, final String label, final Direction direction) {
        Edge edge = getAtMostOneEdge(vertex, label, direction);
        return null == edge ? null : getVertex(edge, direction.opposite());
    }

    private static Vertex getExactlyOneVertex(final Vertex vertex, final String label, final Direction direction) {
        return getVertex(getExactlyOneEdge(vertex, label, direction), direction.opposite());
    }

    static <T> T getExactlyOneEntity(final Vertex vertex,
            final String label, final Direction direction, final Function<Vertex, T> constructor) {
        return constructor.apply(getExactlyOneVertex(vertex, label, direction));
    }

    static <T> T getAtMostOneEntity(final Vertex vertex,
            final String label, final Direction direction, final Function<Vertex, T> constructor) {
        Vertex v = getAtMostOneVertex(vertex, label, direction);
        return null == v ? null : constructor.apply(v);
    }

    private static Edge getAtMostOneEdge(final Vertex vertex, final String label, final Direction direction) {
        Iterator<Edge> iter = vertex.edges(direction, label);
        if (!iter.hasNext()) {
            return null;
        }
        Edge result = iter.next();
        if (iter.hasNext()) {
            throw new InvalidGraphException("vertex " + toString(vertex)
                    + " has more than one '" + label + "' " + direction + " edge");
        }
        return result;
    }

    private static Edge getExactlyOneEdge(final Vertex vertex, final String label, final Direction direction) {
        Edge other = getAtMostOneEdge(vertex, label, direction);
        if (null == other) {
            throw new InvalidGraphException("vertex " + toString(vertex)
                    + " is missing '" + label + "' " + direction + " edge");
        }
        return other;
    }

    static void forEachAdjacentVertex(Vertex vertex, final String label, Direction direction, Consumer<Vertex> consumer) {
        vertex.vertices(direction, label).forEachRemaining(consumer);
    }

    static boolean hasAdjacentVertex(Vertex vertex, final String label, Direction direction) {
        return vertex.vertices(direction, label).hasNext();
    }

    static boolean removeEdge(Vertex vertex, final String label, Direction direction) {
        final Mutable<Boolean> changed = new Mutable<>(false);
        vertex.edges(direction, label).forEachRemaining(
                edge -> {
                    edge.remove();
                    changed.value = true;
                });
        return changed.value;
    }

    static void destroyInternal(final Vertex vertex) {
        vertex.remove();
    }

    static class Mutable<T> {
        public T value;

        public Mutable(T value) {
            this.value = value;
        }
    }

    private static Vertex getVertex(final Edge edge, final Direction direction) {
        switch (direction) {
            case OUT:
                return edge.outVertex();
            case IN:
                return edge.inVertex();
            default:
                throw new IllegalStateException();
        }
    }

    static String toString(final Vertex vertex) {
        return "entity[" + vertex.id() + "]";
    }
}
