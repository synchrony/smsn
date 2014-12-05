package net.fortytwo.extendo.brain;

import com.tinkerpop.blueprints.Index;
import com.tinkerpop.blueprints.KeyIndexableGraph;
import com.tinkerpop.blueprints.Parameter;
import com.tinkerpop.blueprints.Vertex;
import com.tinkerpop.blueprints.util.wrappers.id.IdGraph;
import com.tinkerpop.frames.FramedGraph;
import com.tinkerpop.frames.FramedGraphFactory;
import net.fortytwo.extendo.Extendo;
import net.fortytwo.extendo.util.TypedProperties;
import org.neo4j.index.impl.lucene.LowerCaseKeywordAnalyzer;

import java.util.Collection;
import java.util.Date;
import java.util.Iterator;
import java.util.LinkedList;
import java.util.List;
import java.util.logging.Logger;

/**
 * A graph of atoms and lists conforming to the Extend-o-Brain data model
 *
 * @author Joshua Shinavier (http://fortytwo.net)
 */
public class BrainGraph {
    private static final Logger logger = Extendo.getLogger(BrainGraph.class);

    private final IdGraph<KeyIndexableGraph> graph;

    private final FramedGraph<KeyIndexableGraph> framedGraph;

    // full-text search
    private Index<Vertex> searchIndex;
    // search on first letters, e.g. "ny" finds "New York", "eob" finds "Extend-o-Brain"
    private Index<Vertex> acronymIndex;

    private static final String thingNamespace;

    /**
     * The configurable namespace into which things, i.e. classified atoms, are mapped
     */
    public static final String PROP_THING_NAMESPACE = "net.fortytwo.extendo.brain.thingNamespace";

    private static final String DEFAULT_THING_NAMESPACE = "http://example.org/things/";

    static {
        try {
            thingNamespace = Extendo.getConfiguration().getString(PROP_THING_NAMESPACE, DEFAULT_THING_NAMESPACE);
        } catch (TypedProperties.PropertyException e) {
            throw new ExceptionInInitializerError(e);
        }
    }

    public BrainGraph(final KeyIndexableGraph baseGraph) {
        IdGraph.IdFactory f = new ExtendoIdFactory();
        graph = new IdGraph<KeyIndexableGraph>(baseGraph);
        graph.setVertexIdFactory(f);
        graph.setEdgeIdFactory(f);

        framedGraph = createFramedGraph(graph);

        searchIndex = graph.getIndex("search", Vertex.class);
        if (null == searchIndex) {
            try {
                Class.forName("org.neo4j.index.impl.lucene.LowerCaseKeywordAnalyzer");

                logger.info("creating fulltext search index");
                searchIndex = graph.createIndex(
                        "search", Vertex.class, new Parameter("analyzer", LowerCaseKeywordAnalyzer.class.getName()));
            } catch (ClassNotFoundException e) {
                logger.warning("fulltext search not available");
            }
        }
        acronymIndex = graph.getIndex("acronyms", Vertex.class);
        if (null == acronymIndex) {
            try {
                Class.forName("org.neo4j.index.impl.lucene.LowerCaseKeywordAnalyzer");

                logger.info("creating 'acronym' index");
                acronymIndex = graph.createIndex(
                        "acronyms", Vertex.class, new Parameter("analyzer", LowerCaseKeywordAnalyzer.class.getName()));
            } catch (ClassNotFoundException e) {
                logger.warning("acronym search not available");
            }
        }

        // reverse index of user-defined shortcuts, e.g. "mf" for "my family"
        // shortcuts are distinct from acronyms, which are defined automatically for all values below a certain length
        if (!graph.getIndexedKeys(Vertex.class).contains(Extendo.SHORTCUT)) {
            logger.info("creating key index for '" + Extendo.SHORTCUT + "' property");
            graph.createKeyIndex(Extendo.SHORTCUT, Vertex.class);
        }

        // TODO: alias index is never used
        if (!graph.getIndexedKeys(Vertex.class).contains(Extendo.ALIAS)) {
            logger.info("creating key index for '" + Extendo.ALIAS + "' property");
            graph.createKeyIndex(Extendo.ALIAS, Vertex.class);
        }
    }

    private FramedGraph createFramedGraph(final KeyIndexableGraph baseGraph) {
        // Currently, creating a FramedGraph, rather than via the FramedGraph public constructor,
        // does *not* add GremlinGroovyAnnotationHandler,
        // which is incompatible with Android (due to missing javax.script)
        FramedGraphFactory f = new FramedGraphFactory();
        return f.create(baseGraph);
    }

    public KeyIndexableGraph getPropertyGraph() {
        return graph;
    }

    public FramedGraph<KeyIndexableGraph> getFramedGraph() {
        return framedGraph;
    }

    public static String getId(final Atom a) {
        return (String) a.asVertex().getId();
    }

    public static String uriForId(final String id) {
        return thingNamespace + id;
    }

    public static String uriOf(final Atom a) {
        return uriForId(getId(a));
    }

    private static class ExtendoIdFactory implements IdGraph.IdFactory {
        public String createId() {
            return Extendo.createRandomKey();
        }
    }

    public Vertex getVertex(final String key) {
        return this.getPropertyGraph().getVertex(key);
    }

    public Atom getAtom(final String key) {
        Vertex v = getVertex(key);

        return null == v ? null : getAtom(v);
    }

    public Atom getAtom(final Vertex v) {
        if (null == v) {
            throw new IllegalArgumentException("null vertex");
        }

        return framedGraph.frame(v, Atom.class);
    }

    public Atom createAtom(final Filter filter,
                           final String id) {
        Atom a = framedGraph.frame(this.getPropertyGraph().addVertex(id), Atom.class);
        a.setCreated(new Date().getTime());

        a.setSharability(filter.getDefaultSharability());
        a.setWeight(filter.getDefaultWeight());

        return a;
    }

    public AtomList createAtomList() {
        return framedGraph.frame(this.getPropertyGraph().addVertex(null), AtomList.class);
    }

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

    public void deleteAtom(final Atom a) {
        graph.removeVertex(a.asVertex());
    }

    public void deleteListNode(final AtomList l) {
        graph.removeVertex(l.asVertex());
    }

    public Collection<Atom> getAtomsWithValue(final String value) {
        Collection<Atom> results = new LinkedList<Atom>();

        for (Vertex v : graph.getVertices(Extendo.VALUE, value)) {
            results.add(getAtom(v));
        }

        return results;
    }

    public void indexForSearch(final Atom a,
                               final String value) {
        if (null != searchIndex) {
            // TODO: remove existing values
            searchIndex.put(Extendo.VALUE, value, a.asVertex());
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
                acronymIndex.put(Extendo.ACRONYM, acronym.toString(), a.asVertex());
            }
        }
    }

    /**
     * @return an Iterable of all atoms in the knowledge base, as opposed to all vertices
     * (many of which are list nodes rather than atoms)
     */
    public Iterable<Atom> getAtoms() {
        return new Iterable<Atom>() {
            public Iterator<Atom> iterator() {
                return new Iterator<Atom>() {
                    private Iterator<Vertex> iter = graph.getVertices().iterator();
                    private Atom next = null;

                    public boolean hasNext() {
                        if (null == next) {
                            while (iter.hasNext()) {
                                Vertex v = iter.next();

                                // Here, a vertex is considered an atom if it has a creation timestamp
                                if (null != v.getProperty(Extendo.CREATED)) {
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

    public List<Atom> getAtomsByFulltextQuery(final String query,
                                              final Filter filter) {
        List<Atom> results = new LinkedList<Atom>();

        if (null != searchIndex) {
            for (Vertex v : searchIndex.query(Extendo.VALUE, query)) {
                Atom a = getAtom(v);

                if (null == a) {
                    throw new IllegalStateException("vertex with id " + v.getId() + " is not an atom");
                }

                if (filter.isVisible(v)) {
                    results.add(a);
                }
            }
        }

        return results;
    }

    public List<Atom> getAtomsByAcronymQuery(final String query,
                                             final Filter filter) {
        List<Atom> results = new LinkedList<Atom>();

        if (null != acronymIndex) {
            for (Vertex v : acronymIndex.query(Extendo.ACRONYM, query)) {
                Atom a = getAtom(v);

                if (null == a) {
                    throw new IllegalStateException("vertex with id " + v.getId() + " is not an atom");
                }

                if (filter.isVisible(v)) {
                    results.add(a);
                }
            }
        }

        return results;
    }

    public List<Atom> getAtomsWithShortcut(final String shortcut,
                                           final Filter filter) {
        List<Atom> results = new LinkedList<Atom>();

        for (Vertex v : graph.getVertices(Extendo.SHORTCUT, shortcut)) {
            if (filter.isVisible(v)) {
                results.add(getAtom(v));
            }
        }

        return results;
    }
}
