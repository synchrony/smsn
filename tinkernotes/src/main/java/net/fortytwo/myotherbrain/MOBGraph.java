package net.fortytwo.myotherbrain;

import com.tinkerpop.blueprints.pgm.IndexableGraph;
import com.tinkerpop.frames.FramesManager;
import com.tinkerpop.tinkubator.idindex.IdIndexGraph;

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
}
