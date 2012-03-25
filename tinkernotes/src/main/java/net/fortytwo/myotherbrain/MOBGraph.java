package net.fortytwo.myotherbrain;

import com.tinkerpop.blueprints.pgm.CloseableSequence;
import com.tinkerpop.blueprints.pgm.Index;
import com.tinkerpop.blueprints.pgm.IndexableGraph;
import com.tinkerpop.blueprints.pgm.Vertex;
import com.tinkerpop.frames.FramesManager;
import com.tinkerpop.tinkubator.idindex.IdIndexGraph;
import net.fortytwo.myotherbrain.notes.Filter;

import java.util.Collection;
import java.util.Date;
import java.util.LinkedList;

/**
 * @author Joshua Shinavier (http://fortytwo.net)
 */
public class MOBGraph {
    private final IdIndexGraph graph;

    private final FramesManager manager;

    public MOBGraph(final IndexableGraph baseGraph) {
        graph = new IdIndexGraph(baseGraph, new MOBIdFactory());

        manager = new FramesManager(graph);
    }

    public IndexableGraph getGraph() {
        return graph;
    }

    public FramesManager getManager() {
        return manager;
    }

    public static String getId(final Atom a) {
        return (String) a.asVertex().getId();
    }

    private static class MOBIdFactory implements IdIndexGraph.IdFactory {
        public String createId() {
            return MyOtherBrain.createRandomKey();
        }
    }

    public Atom getAtom(final String key) {
        Vertex v = this.getGraph().getVertex(key);

        return null == v ? null : getAtom(v);
    }

    public Atom getAtom(final Vertex v) {
        if (null == v) {
            throw new IllegalArgumentException("null vertex");
        }

        return manager.frame(v, Atom.class);
    }

    public Atom createAtom(final Filter filter) {
        Atom a = manager.frame(this.getGraph().addVertex(null), Atom.class);
        a.setCreated(new Date().getTime());

        a.setSharability(filter.getDefaultSharability());
        a.setWeight(filter.getDefaultWeight());

        return a;
    }

    public Collection<Atom> getAtomsWithValue(final String value) {
        Collection<Atom> results = new LinkedList<Atom>();

        Index<Vertex> vertices = graph.getIndex(Index.VERTICES, Vertex.class);
        CloseableSequence<Vertex> i = vertices.get(MyOtherBrain.VALUE, value);
        try {
            while (i.hasNext()) {
                results.add(getAtom(i.next()));
            }
        } finally {
            i.close();
        }

        return results;
    }

    public Collection<Atom> getAtomsByFulltextQuery(final String query,
                                                    final Filter filter) {
        Collection<Atom> results = new LinkedList<Atom>();

        // TODO: this relies on a temporary Blueprints hack which only works with Neo4j
        CloseableSequence<Vertex> i = graph.getIndex(Index.VERTICES, Vertex.class).get("value", "%query%" + query);
        try {
            while (i.hasNext()) {
                Atom a = getAtom(i.next());

                if (filter.isVisible(a)) {
                    results.add(a);
                }
            }
        } finally {
            i.close();
        }

        return results;
    }
}
