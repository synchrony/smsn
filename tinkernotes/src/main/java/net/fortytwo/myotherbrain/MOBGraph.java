package net.fortytwo.myotherbrain;

import com.tinkerpop.blueprints.pgm.IndexableGraph;
import com.tinkerpop.blueprints.pgm.Vertex;
import com.tinkerpop.frames.FramesManager;
import com.tinkerpop.tinkubator.idindex.IdIndexGraph;
import net.fortytwo.myotherbrain.notes.Filter;

import java.util.Date;

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
}
