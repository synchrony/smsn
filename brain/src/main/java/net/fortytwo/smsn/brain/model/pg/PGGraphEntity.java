package net.fortytwo.smsn.brain.model.pg;

import com.tinkerpop.blueprints.Direction;
import com.tinkerpop.blueprints.Graph;
import com.tinkerpop.blueprints.Vertex;
import net.fortytwo.smsn.brain.error.InvalidGraphException;
import net.fortytwo.smsn.brain.error.InvalidUpdateException;
import net.fortytwo.smsn.brain.model.Atom;
import net.fortytwo.smsn.brain.model.AtomList;

import java.util.Iterator;
import java.util.function.Consumer;

/**
 * @author Joshua Shinavier (http://fortytwo.net)
 */
abstract class PGGraphEntity {
    protected abstract PGAtomGraph getAtomGraph();

    private final Vertex vertex;

    protected String getId() {
        return (String) vertex.getId();
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

    protected void addOutEdge(final Vertex inVertex, final String label) {
        getPropertyGraph().addEdge(null, asVertex(), inVertex, label);
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

    protected Object getOptionalProperty(String name) {
        return vertex.getProperty(name);
    }

    protected Object getRequiredProperty(String name) {
        Object value = getOptionalProperty(name);
        if (null == value) {
            throw new InvalidGraphException("missing property '" + name + "' for entity " + getId());
        }
        return value;
    }

    private boolean setProperty(String name, Object value) {
        Object previousValue = vertex.getProperty(name);

        if (null == value) {
            if (null == previousValue) {
                return false;
            } else {
                vertex.removeProperty(name);
                return true;
            }
        } else {
            if (null == previousValue || !value.equals(previousValue)) {
                vertex.setProperty(name, value);
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
            throw new InvalidUpdateException("can't clear required property '" + name + "' on entity " + getId());
        }

        return setProperty(name, value);
    }

    protected Vertex getAtMostOneVertex(final String label, Direction direction) {
        Iterator<Vertex> vertexIter = vertex.getVertices(direction, label).iterator();
        if (!vertexIter.hasNext()) {
            return null;
        }
        Vertex result = vertexIter.next();
        if (vertexIter.hasNext()) {
            throw new InvalidGraphException("vertex has more than one '" + label + "' out-edge");
        }
        return result;
    }

    protected Vertex getExactlyOneVertex(final String label, Direction direction) {
        Vertex vertex = getAtMostOneVertex(label, direction);
        if (null == vertex) {
            throw new InvalidGraphException("missing '" + label + "' " + direction + " edge");
        }
        return vertex;
    }

    protected void forEachAdjacentVertex(final String label, Direction direction, Consumer<Vertex> consumer) {
        vertex.getVertices(direction, label).forEach(consumer);
    }

    protected boolean removeEdge(final String label, Direction direction) {
        final Mutable<Boolean> changed = new Mutable<>(false);
        asVertex().getEdges(direction, label).forEach(
                edge -> {getPropertyGraph().removeEdge(edge); changed.value = true;});
        return changed.value;
    }

    private static class Mutable<T> {
        public T value;

        public Mutable(T value) {
            this.value = value;
        }
    }
}
