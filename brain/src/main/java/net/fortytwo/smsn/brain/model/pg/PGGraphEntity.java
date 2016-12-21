package net.fortytwo.smsn.brain.model.pg;

import net.fortytwo.smsn.SemanticSynchrony;
import net.fortytwo.smsn.brain.error.InvalidGraphException;
import net.fortytwo.smsn.brain.error.InvalidUpdateException;
import net.fortytwo.smsn.brain.model.Atom;
import net.fortytwo.smsn.brain.model.AtomList;
import org.apache.tinkerpop.gremlin.structure.Direction;
import org.apache.tinkerpop.gremlin.structure.Edge;
import org.apache.tinkerpop.gremlin.structure.Graph;
import org.apache.tinkerpop.gremlin.structure.Vertex;
import org.apache.tinkerpop.gremlin.structure.VertexProperty;

import java.util.Iterator;
import java.util.function.Consumer;

abstract class PGGraphEntity {
    protected abstract PGAtomGraph getAtomGraph();

    private final Vertex vertex;

    protected String getId() {
        VertexProperty<String> property = vertex.property(SemanticSynchrony.ID_V);
        if (!property.isPresent()) {
            throw new IllegalStateException("missing id");
        }
        return property.value();
    }

    protected Graph getPropertyGraph() {
        return getAtomGraph().getPropertyGraph();
    }

    protected AtomList createList() {
        return getAtomGraph().createAtomList();
    }

    protected PGGraphEntity(Vertex vertex) {
        this.vertex = vertex;
    }

    public Vertex asVertex() {
        return vertex;
    }

    protected void addOutEdge(final Object id, final Vertex inVertex, final String label) {
        // TODO: control the id of the edge
        asVertex().addEdge(label, inVertex);
    }

    protected Atom asAtom(Vertex vertex) {
        return vertexAsAtom(vertex);
    }

    protected AtomList asAtomList(Vertex vertex) {
        return vertexAsAtomList(vertex);
    }

    protected Atom vertexAsAtom(final Vertex vertex) {
        PGAtomGraph atomGraph = getAtomGraph();
        return null == vertex ? null : new PGAtom(vertex) {
            @Override
            protected PGAtomGraph getAtomGraph() {
                return atomGraph;
            }
        };
    }

    protected AtomList vertexAsAtomList(final Vertex vertex) {
        PGAtomGraph atomGraph = getAtomGraph();
        return null == vertex ? null : new PGAtomList(vertex) {
            @Override
            protected PGAtomGraph getAtomGraph() {
                return atomGraph;
            }
        };
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
            throw new InvalidGraphException("missing property '" + name + "' for atom vertex " + getId());
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
            throw new InvalidUpdateException("can't clear required property '" + name
                    + "' on atom vertex " + getId());
        }

        return setProperty(name, value);
    }

    protected void forAllVertices(final String label, final Direction direction, final Consumer<AtomList> consumer) {
        vertex.vertices(direction, label).forEachRemaining(vertex -> consumer.accept(vertexAsAtomList(vertex)));
    }

    protected Vertex getAtMostOneVertex(final String label, final Direction direction) {
        Edge edge = getAtMostOneEdge(label, direction);
        return null == edge ? null : getVertex(edge, direction.opposite());
    }

    protected Vertex getExactlyOneVertex(final String label, final Direction direction) {
        return getVertex(getExactlyOneEdge(label, direction), direction.opposite());
    }

    protected Edge getAtMostOneEdge(final String label, final Direction direction) {
        Iterator<Edge> iter = vertex.edges(direction, label);
        if (!iter.hasNext()) {
            return null;
        }
        Edge result = iter.next();
        if (iter.hasNext()) {
            throw new InvalidGraphException("atom vertex " + getId()
                    + " has more than one '" + label + "' " + direction + " edge");
        }
        return result;
    }

    protected Edge getExactlyOneEdge(final String label, final Direction direction) {
        Edge other = getAtMostOneEdge(label, direction);
        if (null == other) {
            throw new InvalidGraphException("atom vertex " + getId()
                    + " is missing '" + label + "' " + direction + " edge");
        }
        return other;
    }

    protected void forEachAdjacentVertex(final String label, Direction direction, Consumer<Vertex> consumer) {
        vertex.vertices(direction, label).forEachRemaining(consumer);
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
}
