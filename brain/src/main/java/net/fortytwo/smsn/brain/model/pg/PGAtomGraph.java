package net.fortytwo.smsn.brain.model.pg;

import net.fortytwo.smsn.SemanticSynchrony;
import net.fortytwo.smsn.brain.model.Atom;
import net.fortytwo.smsn.brain.model.AtomGraph;
import net.fortytwo.smsn.brain.model.AtomList;
import net.fortytwo.smsn.brain.model.Filter;
import net.fortytwo.smsn.util.TypedProperties;
import org.apache.tinkerpop.gremlin.structure.Direction;
import org.apache.tinkerpop.gremlin.structure.Graph;
import org.apache.tinkerpop.gremlin.structure.T;
import org.apache.tinkerpop.gremlin.structure.Vertex;
import org.apache.tinkerpop.gremlin.structure.VertexProperty;
import org.apache.tinkerpop.gremlin.tinkergraph.structure.TinkerGraph;

import java.util.Date;
import java.util.Iterator;
import java.util.LinkedList;
import java.util.List;

public class PGAtomGraph implements AtomGraph {

    private static final String REDACTED_VALUE = "";

    private static final String thingNamespace;

    static {
        try {
            thingNamespace = SemanticSynchrony.getConfiguration().getString(PROP_THING_NAMESPACE, DEFAULT_THING_NAMESPACE);
        } catch (TypedProperties.PropertyException e) {
            throw new ExceptionInInitializerError(e);
        }
    }

    private final GraphWrapper wrapper;
    private final Graph propertyGraph;
    private final PGAtomGraph thisGraph;

    private long lastUpdate;

    public PGAtomGraph(final GraphWrapper wrapper) {
        this.wrapper = wrapper;
        this.propertyGraph = wrapper.getGraph();
        thisGraph = this;
    }

    public Graph getPropertyGraph() {
        return propertyGraph;
    }

    public String idOfAtom(final Atom a) {
        return a.getId();
    }

    // TODO: move me
    public static String iriForId(final String id) {
        return thingNamespace + id;
    }

    public String iriOfAtom(final Atom a) {
        return iriForId(idOfAtom(a));
    }

    @Override
    public long getLastUpdate() {
        return lastUpdate;
    }

    @Override
    public void commit() {
        propertyGraph.tx().commit();
    }

    @Override
    public AtomGraph createFilteredGraph(Filter filter) {
        //return new FilteredAtomGraph(this, filter);
        return copyGraph(filter);
    }

    @Override
    public void notifyOfUpdate() {
        this.lastUpdate = System.currentTimeMillis();
    }

    @Override
    public Atom getAtom(final String key) {
        Vertex v = getVertex(propertyGraph, key);

        return null == v ? null : getAtom(v);
    }

    public Atom getAtom(final Vertex v) {
        if (null == v) {
            throw new IllegalArgumentException("null vertex");
        }

        return new PGAtomImpl(v);
    }

    @Override
    public AtomList createAtomList(String id) {
        Vertex vertex = createVertex(id, SemanticSynchrony.ATOM_LIST);
        return new PGAtomListImpl(vertex);
    }

    @Override
    public AtomList createAtomList(final Atom... elements) {
        if (0 == elements.length) {
            throw new IllegalArgumentException("empty list");
        }

        AtomList last = null;
        AtomList head = null;
        for (Atom a : elements) {
            AtomList cur = createAtomList();
            if (null == head) {
                head = cur;
            }
            if (last != null) {
                last.setRest(cur);
            }
            cur.setFirst(a);
            last = cur;
        }

        return head;
    }

    @Override
    public Atom createAtom(final Filter filter,
                           final String id) {

        Vertex vertex = createVertex(id, SemanticSynchrony.ATOM);
        Atom atom = new PGAtomImpl(vertex);
        atom.setCreated(new Date().getTime());

        atom.setSharability(filter.getDefaultSharability());
        atom.setWeight(filter.getDefaultWeight());

        return atom;
    }

    public AtomList createAtomList() {
        return createAtomList((String) null);
    }

    @Override
    public void removeIsolatedAtoms(final Filter filter) {
        if (null == filter) {
            throw new IllegalArgumentException();
        }

        List<Vertex> toRemove = new LinkedList<>();

        propertyGraph.vertices().forEachRemaining(v -> {
            if (null != v.property("value")
                    && !v.edges(Direction.IN).hasNext()
                    && !v.edges(Direction.OUT).hasNext()) {
                Atom a = getAtom(v);
                if (filter.isVisible(a)) {
                    toRemove.add(v);
                }
            }
        });

        for (Vertex v : toRemove) {
            // note: we assume from the above that there are no dependent vertices (i.e. list nodes) to remove first
            v.remove();
        }

        notifyOfUpdate();
    }

    @Override
    public void reindexAtom(final Atom atom) {
        Vertex vertex = ((PGAtom) atom).asVertex();

        updateAcronym((PGAtom) atom, vertex);

        wrapper.reindex(vertex);
    }

    /**
     * @return an Iterable of all atoms in the knowledge base, as opposed to all vertices
     * (many of which are list nodes rather than atoms)
     */
    @Override
    public Iterable<Atom> getAllAtoms() {
        return () -> new Iterator<Atom>() {
            private final Iterator<Vertex> iter = getPropertyGraph().vertices();
            private Atom next = null;

            public boolean hasNext() {
                if (null == next) {
                    while (iter.hasNext()) {
                        Vertex v = iter.next();

                        // Here, a vertex is considered an atom if it has a creation timestamp
                        if (v.property(SemanticSynchrony.CREATED).isPresent()) {
                            next = getAtom(v);
                            break;
                        }
                    }

                    return null != next;
                } else {
                    return true;
                }
            }

            public Atom next() {
                hasNext();
                Atom tmp = next;
                next = null;
                return tmp;
            }

            public void remove() {
                throw new UnsupportedOperationException();
            }
        };
    }

    @Override
    public List<Atom> getAtomsByValue(final String value, final Filter filter) {
        return filterVerticesToAtoms(wrapper.queryByValue(value), filter);
    }

    @Override
    public List<Atom> getAtomsByAcronym(final String acronym, final Filter filter) {
        return filterVerticesToAtoms(wrapper.queryByAcronym(acronym), filter);
    }

    @Override
    public List<Atom> getAtomsByShortcut(final String shortcut, final Filter filter) {
        return filterVerticesToAtoms(wrapper.queryByShortcut(shortcut), filter);
    }

    private void updateAcronym(final PGAtom atom, final Vertex asVertex) {
        String value = atom.getValue();
        String acronym = valueToAcronym(value);

        VertexProperty<String> previousProperty = asVertex.property(SemanticSynchrony.ACRONYM);
        if (null != previousProperty) {
            previousProperty.remove();
        }

        if (null != acronym) {
            asVertex.property(SemanticSynchrony.ACRONYM, acronym);
        }
    }

    private String valueToAcronym(final String value) {
        // index only short, name-like values, avoiding free-form text if possible
        if (value.length() <= 100) {
            String clean = value.toLowerCase().replaceAll("[-_\t\n\r]", " ").trim();
            StringBuilder acronym = new StringBuilder();
            boolean isInside = false;
            for (byte b : clean.getBytes()) {
                // TODO: support international letter characters as such
                if (b >= 'a' && b <= 'z') {
                    if (!isInside) {
                        acronym.append((char) b);
                        isInside = true;
                    }
                } else if (' ' == b) {
                    isInside = false;
                }
            }

            return acronym.toString();
        } else {
            return null;
        }
    }

    public PGAtomGraph copyGraph(final Filter filter) {
        Object edgeId;
        GraphWrapper newWrapper = new TinkerGraphWrapper(TinkerGraph.open());
        PGAtomGraph newGraph = new PGAtomGraph(newWrapper);

        for (Atom originalAtom : getAllAtoms()) {
            if (filter.isVisible(originalAtom)) {
                PGAtom newAtom = findOrCopyAtom(originalAtom, filter, newGraph);
                PGAtomList notes = (PGAtomList) originalAtom.getNotes();
                if (null != notes) {
                    edgeId = getOutEdgeId((PGAtom) originalAtom, SemanticSynchrony.NOTES);
                    newAtom.setNotes(copyAtomList(notes, filter, newGraph), edgeId);
                }
            }
        }

        return newGraph;
    }

    private List<Atom> filterVerticesToAtoms(final Iterator<Vertex> vertices, final Filter filter) {
        List<Atom> results = new LinkedList<>();

        vertices.forEachRemaining(v -> {
            Atom a = getAtom(v);

            if (filter.isVisible(a)) {
                results.add(a);
            }
        });

        return results;
    }

    private PGAtom findOrCopyAtom(final Atom original, final Filter filter, final AtomGraph newGraph) {
        PGAtom newAtom = (PGAtom) newGraph.getAtom(original.getId());
        if (null != newAtom) return newAtom;

        newAtom = (PGAtom) newGraph.createAtom(filter, original.getId());
        newAtom.setSharability(original.getSharability());

        if (filter.isVisible(original)) {
            newAtom.setValue(original.getValue());
            newAtom.setWeight(original.getWeight());
            newAtom.setShortcut(original.getShortcut());
            newAtom.setPriority(original.getPriority());
            newAtom.setAlias(original.getAlias());
            newAtom.setCreated(original.getCreated());
        } else {
            newAtom.setValue(REDACTED_VALUE);
        }

        return newAtom;
    }

    private AtomList copyAtomList(final PGAtomList original, final Filter filter, final PGAtomGraph newGraph) {
        Object edgeId;
        PGAtomList originalCur = original, originalPrev = null;
        PGAtomList newHead = null, newCur, newPrev = null;
        while (null != originalCur) {
            newCur = (PGAtomList) newGraph.createAtomList(originalCur.getId());
            edgeId = getOutEdgeId(originalCur, SemanticSynchrony.FIRST);
            Atom originalFirst = originalCur.getFirst();
            PGAtom newAtom = findOrCopyAtom(originalFirst, filter, newGraph);
            newCur.setFirst(newAtom, edgeId);

            if (null == newPrev) {
                newHead = newCur;
            } else {
                edgeId = getOutEdgeId(originalPrev, SemanticSynchrony.REST);
                newPrev.setRest(newCur, edgeId);
            }

            newPrev = newCur;
            originalPrev = originalCur;
            originalCur = (PGAtomList) originalCur.getRest();
        }

        return newHead;
    }

    private Object getOutEdgeId(final PGGraphEntity entity, final String label) {
        Object id = entity.getExactlyOneEdge(label, Direction.OUT).id();
        return id;
    }

    private Vertex getVertex(Graph graph, String id) {
        // note: requires a key index for efficiency
        Iterator<Vertex> vertices = graph.traversal().V().has(SemanticSynchrony.ID, id);
        return vertices.hasNext() ? vertices.next() : null;
    }

    private Vertex createVertex(final String id, final String label) {
        Vertex vertex = propertyGraph.addVertex(T.label, label);
        // TODO: use id strategy
        vertex.property(SemanticSynchrony.ID, getNonNullId(id));

        return vertex;
    }

    private String getNonNullId(final String id) {
        return null == id ? SemanticSynchrony.createRandomKey() : id;
    }

    private class PGAtomImpl extends PGAtom {
        protected PGAtomImpl(Vertex vertex) {
            super(vertex);
        }

        @Override
        protected PGAtomGraph getAtomGraph() {
            return thisGraph;
        }
    }

    private class PGAtomListImpl extends PGAtomList {
        public PGAtomListImpl(Vertex vertex) {
            super(vertex);
        }

        @Override
        protected PGAtomGraph getAtomGraph() {
            return thisGraph;
        }
    }
}
