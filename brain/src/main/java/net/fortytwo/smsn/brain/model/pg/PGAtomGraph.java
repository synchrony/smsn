package net.fortytwo.smsn.brain.model.pg;

import com.tinkerpop.blueprints.Direction;
import com.tinkerpop.blueprints.Index;
import com.tinkerpop.blueprints.KeyIndexableGraph;
import com.tinkerpop.blueprints.Parameter;
import com.tinkerpop.blueprints.Vertex;
import com.tinkerpop.blueprints.impls.tg.TinkerGraph;
import com.tinkerpop.blueprints.util.wrappers.id.IdGraph;
import net.fortytwo.smsn.SemanticSynchrony;
import net.fortytwo.smsn.brain.model.Atom;
import net.fortytwo.smsn.brain.model.AtomGraph;
import net.fortytwo.smsn.brain.model.AtomList;
import net.fortytwo.smsn.brain.model.Filter;
import net.fortytwo.smsn.brain.model.filtered.FilteredAtomGraph;
import net.fortytwo.smsn.util.TypedProperties;
import org.neo4j.index.impl.lucene.LowerCaseKeywordAnalyzer;

import java.util.Date;
import java.util.Iterator;
import java.util.LinkedList;
import java.util.List;
import java.util.logging.Logger;

public class PGAtomGraph implements AtomGraph {
    private static final Logger logger = SemanticSynchrony.getLogger(PGAtomGraph.class);

    private static final String REDACTED_VALUE = "";

    private static final String thingNamespace;

    static {
        try {
            thingNamespace = SemanticSynchrony.getConfiguration().getString(PROP_THING_NAMESPACE, DEFAULT_THING_NAMESPACE);
        } catch (TypedProperties.PropertyException e) {
            throw new ExceptionInInitializerError(e);
        }
    }

    private static IdGraph<KeyIndexableGraph> createIdGraph(final KeyIndexableGraph baseGraph) {
        IdGraph.IdFactory f = new AtomIdFactory();
        IdGraph<KeyIndexableGraph> idGraph = new IdGraph<>(baseGraph);
        idGraph.setVertexIdFactory(f);
        idGraph.setEdgeIdFactory(f);
        return idGraph;
    }

    private final IdGraph<KeyIndexableGraph> propertyGraph;
    private final PGAtomGraph thisGraph;

    // full-text search
    private Index<Vertex> searchIndex;
    // search on first letters, e.g. "ny" finds "New York", "eob" finds "Extend-o-Brain"
    private Index<Vertex> acronymIndex;

    private long lastUpdate;

    public PGAtomGraph(final KeyIndexableGraph baseGraph) {
        this.propertyGraph = createIdGraph(baseGraph);
        thisGraph = this;

        searchIndex = getPropertyGraph().getIndex("search", Vertex.class);
        if (null == searchIndex) {
            try {
                Class.forName("org.neo4j.index.impl.lucene.LowerCaseKeywordAnalyzer");

                logger.info("creating fulltext search index");
                searchIndex = getPropertyGraph().createIndex(
                        "search", Vertex.class, new Parameter("analyzer", LowerCaseKeywordAnalyzer.class.getName()));
            } catch (ClassNotFoundException e) {
                logger.warning("fulltext search not available");
            }
        }
        acronymIndex = getPropertyGraph().getIndex("acronyms", Vertex.class);
        if (null == acronymIndex) {
            try {
                Class.forName("org.neo4j.index.impl.lucene.LowerCaseKeywordAnalyzer");

                logger.info("creating 'acronym' index");
                acronymIndex = getPropertyGraph().createIndex(
                        "acronyms", Vertex.class, new Parameter("analyzer", LowerCaseKeywordAnalyzer.class.getName()));
            } catch (ClassNotFoundException e) {
                logger.warning("acronym search not available");
            }
        }

        // reverse index of user-defined shortcuts, e.g. "mf" for "my family"
        // shortcuts are distinct from acronyms, which are defined automatically for all values below a certain length
        if (!getPropertyGraph().getIndexedKeys(Vertex.class).contains(SemanticSynchrony.SHORTCUT)) {
            logger.info("creating key index for '" + SemanticSynchrony.SHORTCUT + "' property");
            getPropertyGraph().createKeyIndex(SemanticSynchrony.SHORTCUT, Vertex.class);
        }

        // TODO: alias index is never used
        if (!getPropertyGraph().getIndexedKeys(Vertex.class).contains(SemanticSynchrony.ALIAS)) {
            logger.info("creating key index for '" + SemanticSynchrony.ALIAS + "' property");
            getPropertyGraph().createKeyIndex(SemanticSynchrony.ALIAS, Vertex.class);
        }
    }

    public IdGraph<KeyIndexableGraph> getPropertyGraph() {
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
        propertyGraph.commit();
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

    private static class AtomIdFactory implements IdGraph.IdFactory {
        public String createId() {
            return SemanticSynchrony.createRandomKey();
        }
    }

    public Vertex getVertex(final String key) {
        return this.getPropertyGraph().getVertex(key);
    }

    @Override
    public Atom getAtom(final String key) {
        Vertex v = getVertex(key);

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
        Vertex vertex = createVertex(id);
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

        Vertex vertex = createVertex(id);
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

        for (Vertex v : propertyGraph.getVertices()) {
            if (null != v.getProperty("value")
                    && !v.getEdges(Direction.IN).iterator().hasNext()
                    && !v.getEdges(Direction.OUT).iterator().hasNext()) {
                Atom a = getAtom(v);
                if (filter.isVisible(a)) {
                    toRemove.add(v);
                }
            }
        }

        for (Vertex v : toRemove) {
            // note: we assume from the above that there are no dependent vertices (i.e. list nodes) to remove first
            propertyGraph.removeVertex(v);
        }

        notifyOfUpdate();
    }

    @Override
    public void addAtomToIndices(final Atom atom) {
        String value = atom.getValue();

        if (null != searchIndex) {
            // TODO: remove existing values
            searchIndex.put(SemanticSynchrony.VALUE, value, ((PGGraphEntity) atom).asVertex());
        }

        if (null != acronymIndex) {
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

                // TODO: remove existing values
                acronymIndex.put(SemanticSynchrony.ACRONYM, acronym.toString(), ((PGGraphEntity) atom).asVertex());
            }
        }
    }

    /**
     * @return an Iterable of all atoms in the knowledge base, as opposed to all vertices
     * (many of which are list nodes rather than atoms)
     */
    @Override
    public Iterable<Atom> getAllAtoms() {
        return new Iterable<Atom>() {
            public Iterator<Atom> iterator() {
                return new Iterator<Atom>() {
                    private final Iterator<Vertex> iter = getPropertyGraph().getVertices().iterator();
                    private Atom next = null;

                    public boolean hasNext() {
                        if (null == next) {
                            while (iter.hasNext()) {
                                Vertex v = iter.next();

                                // Here, a vertex is considered an atom if it has a creation timestamp
                                if (null != v.getProperty(SemanticSynchrony.CREATED)) {
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
        };
    }

    @Override
    public List<Atom> getAtomsByFulltextQuery(final String query, final Filter filter) {
        List<Atom> results = new LinkedList<>();

        if (null != searchIndex) {
            for (Vertex v : searchIndex.query(SemanticSynchrony.VALUE, query)) {
                Atom a = getAtom(v);

                if (null == a) {
                    throw new IllegalStateException("vertex with id " + v.getId() + " is not an atom");
                }

                if (filter.isVisible(a)) {
                    results.add(a);
                }
            }
        }

        return results;
    }

    @Override
    public List<Atom> getAtomsByAcronymQuery(final String query, final Filter filter) {
        List<Atom> results = new LinkedList<>();

        if (null != acronymIndex) {
            for (Vertex v : acronymIndex.query(SemanticSynchrony.ACRONYM, query)) {
                Atom a = getAtom(v);

                if (null == a) {
                    throw new IllegalStateException("vertex with id " + v.getId() + " is not an atom");
                }

                if (filter.isVisible(a)) {
                    results.add(a);
                }
            }
        }

        return results;
    }

    @Override
    public List<Atom> getAtomsWithShortcut(final String shortcut, final Filter filter) {
        List<Atom> results = new LinkedList<>();

        for (Vertex v : getPropertyGraph().getVertices(SemanticSynchrony.SHORTCUT, shortcut)) {
            Atom a = getAtom(v);
            if (filter.isVisible(a)) {
                results.add(getAtom(v));
            }
        }

        return results;
    }

    public PGAtomGraph copyGraph(final Filter filter) {
        String edgeId;
        PGAtomGraph newGraph = new PGAtomGraph(new TinkerGraph());

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
        String edgeId;
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

    private String getOutEdgeId(final PGGraphEntity entity, final String label) {
        return (String) entity.getExactlyOneEdge(label, Direction.OUT).getId();
    }

    private Vertex createVertex(final String id) {
        return propertyGraph.addVertex(id);
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
