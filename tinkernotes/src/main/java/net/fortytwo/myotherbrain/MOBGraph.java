package net.fortytwo.myotherbrain;

import com.tinkerpop.blueprints.pgm.CloseableSequence;
import com.tinkerpop.blueprints.pgm.Index;
import com.tinkerpop.blueprints.pgm.IndexableGraph;
import com.tinkerpop.blueprints.pgm.Vertex;
import com.tinkerpop.frames.FramesManager;
import com.tinkerpop.tinkubator.idindex.IdIndexGraph;
import net.fortytwo.myotherbrain.notes.Filter;

import java.io.BufferedReader;
import java.io.File;
import java.io.FileInputStream;
import java.io.FileOutputStream;
import java.io.IOException;
import java.io.InputStream;
import java.io.InputStreamReader;
import java.io.OutputStream;
import java.io.PrintStream;
import java.util.Collection;
import java.util.Date;
import java.util.HashMap;
import java.util.LinkedList;
import java.util.Map;

/**
 * @author Joshua Shinavier (http://fortytwo.net)
 */
public class MOBGraph {
    private final IdIndexGraph graph;

    private final FramesManager manager;

    private final Map<Object, Integer> attention;
    private final File statsDir;

    private static final Map<IndexableGraph, MOBGraph> graphs = new HashMap<IndexableGraph, MOBGraph>();

    public static MOBGraph getInstance(final IndexableGraph baseGraph) throws Exception {
        MOBGraph g = graphs.get(baseGraph);

        if (null == g) {
            g = new MOBGraph(baseGraph);
            graphs.put(baseGraph, g);
        }

        return g;
    }

    private MOBGraph(final IndexableGraph baseGraph) throws Exception {
        graph = new IdIndexGraph(baseGraph, new MOBIdFactory());

        manager = new FramesManager(graph);

        statsDir = MyOtherBrain.getConfiguration().getFile(MyOtherBrain.STATS_DIRECTORY, null);
        if (null == statsDir) {
            attention = null;
        } else {
            attention = new HashMap<Object, Integer>();
            loadStats();
        }
    }

    private void loadStats() throws IOException {
        File f = new File(statsDir, "attention");
        if (f.exists()) {
            InputStream in = new FileInputStream(f);
            try {
                BufferedReader br = new BufferedReader(new InputStreamReader(in));

                String line;
                while (null != (line = br.readLine())) {
                    String[] a = line.split(",");
                    String id = a[0];
                    Integer count = Integer.valueOf(a[1]);
                    attention.put(id, count);
                }
            } finally {
                in.close();
            }
        }
    }

    public void saveStats() throws IOException {
        if (null != attention) {
            OutputStream out = new FileOutputStream(new File(statsDir, "attention"));
            PrintStream ps = new PrintStream(out);
            try {
                for (Map.Entry<Object, Integer> e : attention.entrySet()) {
                    ps.println("" + e.getKey() + "," + e.getValue());
                }
            } finally {
                out.close();
            }
        }
    }

    public void registerVisit(final Atom a) {
        if (null != attention) {
            Object id = a.asVertex().getId();

            Integer count = attention.get(id);
            if (null == count) {
                count = 0;
            }

            //System.out.println("putting: " + id + ", " + (count + 1));
            attention.put(id, count + 1);
        }
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
