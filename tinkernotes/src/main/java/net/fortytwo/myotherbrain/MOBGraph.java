package net.fortytwo.myotherbrain;

import com.tinkerpop.blueprints.pgm.CloseableSequence;
import com.tinkerpop.blueprints.pgm.Element;
import com.tinkerpop.blueprints.pgm.Index;
import com.tinkerpop.blueprints.pgm.IndexableGraph;
import com.tinkerpop.blueprints.pgm.Vertex;
import com.tinkerpop.frames.FramesManager;

import java.util.HashSet;
import java.util.Set;
import java.util.logging.Logger;

/**
 * @author Joshua Shinavier (http://fortytwo.net)
 */
public class MOBGraph {
    private static final Logger LOGGER = Logger.getLogger(MOBGraph.class.getName());

    private static final String KEYS = "keys";

    private static final int RANDOM_KEY_MAXTRIALS = 100;

    private final IndexableGraph graph;

    private final Index<Vertex> keys;

    private final FramesManager manager;

    public MOBGraph(final IndexableGraph graph) {
        this.graph = graph;

        // TODO: it would be more convenient if IndexableGraph would return a null (with getIndex) for a non-existent index, instead of throwing an exception
        boolean indexExists = false;
        for (Index<? extends Element> index : graph.getIndices()) {
            if (index.getIndexName().equals(KEYS)) {
                indexExists = true;
                break;
            }
        }

        if (!indexExists) {
            LOGGER.warning("'" + KEYS + "' index does not exist. Creating it.");
            Set<String> keys = new HashSet<String>();
            keys.add(MyOtherBrain.KEY);
            graph.createAutomaticIndex(KEYS, Vertex.class, keys);
        }

        keys = graph.getIndex(KEYS, Vertex.class);

        manager = new FramesManager(graph);

        // TODO: temporary
        //migrateKeys();
    }

    public IndexableGraph getGraph() {
        return graph;
    }

    public FramesManager getManager() {
        return manager;
    }

    public Vertex getAtomVertex(final String key) {
        if (null == key) {
            throw new IllegalArgumentException("null atom key");
        }

        Vertex v;
        CloseableSequence<Vertex> s = keys.get(MyOtherBrain.KEY, key);

        try {
            if (!s.hasNext()) {
                return null;
            }
            v = s.next();
            if (s.hasNext()) {
                throw new IllegalStateException("multiple vertices with the same key: '" + key + "'");
            }
        } finally {
            s.close();
        }

        return v;
    }

    /*
        For 5-digit numbers of base 64, expect a collision after 32768 trials (on average).
        There are 1,073,741,824 possibilities.

        int base = 64;
        int length = 5;
        BigDecimal poss = new BigDecimal(base).pow(length);
        BigDecimal trials = new BigDecimal(Math.sqrt((double) base)).pow(length);
        System.out.println("For " + length + "-digit numbers of base " + base + ", expect a collision after "
                + trials + " trials (on average).  There are " + poss + " possibilities.");
     */
    public String createKey() {
        for (int j = 0; j < RANDOM_KEY_MAXTRIALS; j++) {
            String key = MyOtherBrain.createRandomKey();
            if (null == getAtomVertex(key)) {
                return key;
            }
        }

        throw new IllegalStateException("no unoccupied keys have been found");
    }

    // Used irregularly
    private void migrateKeys() {
        for (Vertex v : graph.getVertices()) {
            v.setProperty(MyOtherBrain.KEY, createKey());
        }
    }
}
