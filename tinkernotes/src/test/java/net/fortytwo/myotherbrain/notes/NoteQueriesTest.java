package net.fortytwo.myotherbrain.notes;

import com.tinkerpop.blueprints.Edge;
import com.tinkerpop.blueprints.KeyIndexableGraph;
import com.tinkerpop.blueprints.Vertex;
import com.tinkerpop.blueprints.impls.tg.TinkerGraph;
import com.tinkerpop.frames.FramedGraph;
import net.fortytwo.myotherbrain.ActivityLog;
import net.fortytwo.myotherbrain.Atom;
import net.fortytwo.myotherbrain.AtomList;
import net.fortytwo.myotherbrain.MOBGraph;
import org.json.JSONObject;
import org.junit.After;
import org.junit.Before;
import org.junit.Test;

import java.util.LinkedList;
import java.util.List;

import static junit.framework.Assert.assertEquals;
import static org.junit.Assert.assertArrayEquals;
import static org.junit.Assert.assertNull;

/**
 * @author Joshua Shinavier (http://fortytwo.net)
 */
public class NoteQueriesTest {
    private KeyIndexableGraph graph;
    private MOBGraph store;
    private FramedGraph<KeyIndexableGraph> manager;
    private NoteParser parser;
    private NoteWriter writer = new NoteWriter();
    private NoteQueries semantics;
    private ActivityLog log = null;

    @Before
    public void setUp() throws Exception {
        TinkerGraph g = new TinkerGraph();
        parser = new NoteParser();
        store = MOBGraph.getInstance(g);
        graph = store.getGraph();
        manager = store.getFramedGraph();
        semantics = new NoteQueries(store);
    }

    @After
    public void tearDown() throws Exception {
        graph.shutdown();
    }

    @Test
    public void testEncoding() throws Exception {
        Filter filter = new Filter(0f, 1f, 0.5f, 0f, 1f, 0.5f);
        NoteQueries.AdjacencyStyle style = NoteQueries.FORWARD_ADJACENCY;
        ActivityLog log = null;

        Atom root = createAtom("11111");
        assertEquals("11111", root.asVertex().getId());

        //Note superRoot = new Note();
        Note rootNote = new Note();
        //superRoot.addChild(rootNote);
        rootNote.setValue("foo");
        Note child = new Note();
        child.setValue("cheval \u00e0 phynances");
        rootNote.addChild(child);
        assertNull(child.getWeight());
        assertNull(child.getSharability());
        assertNull(child.getCreated());
        //System.out.println(before.getTargetValue());

        semantics.update(root, rootNote, 1, filter, style, log);

        //new GraphMLWriter(graph).outputGraph(System.out);

        Note after = semantics.view(root, 1, filter, style, log);

        assertEquals("11111", after.getId());
        assertEquals("foo", after.getValue());
        assertEquals(1, after.getChildren().size());

        JSONObject json = writer.toJSON(after);
        //System.out.println(json.toString());
        JSONObject j = json.getJSONArray("children").getJSONObject(0);
        assertEquals("cheval \u00e0 phynances", j.getJSONObject("target").getString("value"));
    }

    @Test
    public void testUpdateRecursion() throws Exception {
        Filter filter = new Filter(0f, 1f, 0.5f, 0f, 1f, 0.5f);
        NoteQueries.AdjacencyStyle style = NoteQueries.FORWARD_ADJACENCY;
        Atom root = createAtom("wXu5g4v");
        String s;

        s = "" +
                "N5KBOAq: ► one\n" +
                "v8EuMtl: ► two\n" +
                "tOpwKho: ► three\n";
        semantics.update(root, parser.parse(s), 2, filter, style, log);
        assertNotesEqual(root, "one", "two", "three");

        Atom one = store.getAtom("N5KBOAq");
        Atom two = store.getAtom("v8EuMtl");

        s = "" +
                "N5KBOAq: ► one\n" +
                "r4zU45R:     ► ten\n" +
                "             ► yellow\n" +
                "tOpwKho: ► three\n";
        semantics.update(root, parser.parse(s), 2, filter, style, log);
        // "two" has been removed
        assertNotesEqual(root, "one", "three");
        // grandchildren have been added
        assertNotesEqual(one, "ten", "yellow");
        Atom ten = store.getAtom("r4zU45R");

        s = "" +
                "N5KBOAq: ► one\n" +
                "r4zU45R:     ► ten\n" +
                "                 ► rabbit\n" +
                "             ► green\n" +
                "tOpwKho: ► three\n";
        semantics.update(root, parser.parse(s), 2, filter, style, log);
        // depth is only two, so "rabbit" is not reachable
        assertNotesEqual(ten);

        s = "" +
                "v8EuMtl: ► two\n" +
                "tOpwKho: ► three\n";
        semantics.update(root, parser.parse(s), 3, filter, style, log);
        // "one" has been removed...
        assertNotesEqual(root, "two", "three");
        // but "one" still exists and has its previous notes
        assertNotesEqual(one, "ten", "green");

        s = "" +
                "v8EuMtl: ► two\n" +
                "             ► elephant\n" +
                "v8EuMtl: ► two\n" +
                "tOpwKho: ► three\n";
        semantics.update(root, parser.parse(s), 3, filter, style, log);
        // duplicates are possible...
        assertNotesEqual(root, "two", "two", "three");
        // ...but when a duplicate is added, children of any matching duplicate will be ignored
        assertNotesEqual(two);

        s = "" +
                "v8EuMtl: ► two\n" +
                "             ► elephant\n" +
                "v8EuMtl: ► two\n" +
                "             ► gorilla\n" +
                "tOpwKho: ► three\n";
        semantics.update(root, parser.parse(s), 3, filter, style, log);
        assertNotesEqual(root, "two", "two", "three");
        // when duplicates already exist, children of duplicates follow the last-occurring instance
        assertNotesEqual(two, "gorilla");
    }

    @Test
    public void testPropertySetting() throws Exception {
        Filter filter = new Filter(0f, 1f, 0.5f, 0f, 1f, 0.5f);
        NoteQueries.AdjacencyStyle style = NoteQueries.FORWARD_ADJACENCY;
        Atom root = createAtom("wXu5g4v");
        String s;

        s = "" +
                "N5KBOAq: ► one\n";
        semantics.update(root, parser.parse(s), 2, filter, style, log);
        Atom one = store.getAtom("N5KBOAq");
        assertEquals(0.5f, one.getWeight());
        assertEquals(0.5f, one.getSharability());
        assertNull(one.getAlias());

        s = "" +
                "N5KBOAq: ► one\n" +
                "             @weight 0.75\n" +
                "             @sharability 0.25\n" +
                "             @alias http://example.org/ns/one\n";
        semantics.update(root, parser.parse(s), 2, filter, style, log);
        assertEquals(0.75f, one.getWeight());
        assertEquals(0.25f, one.getSharability());
        assertEquals("http://example.org/ns/one", one.getAlias());

        s = "" +
                "N5KBOAq: ► one\n" +
                "             @alias \n";
        semantics.update(root, parser.parse(s), 2, filter, style, log);
        assertNull(one.getAlias());
    }

    private void assertNotesEqual(final Atom a,
                                  final String... expected) {
        String[] actual = new String[countNotes(a)];

        int i = 0;
        AtomList cur = a.getNotes();
        while (null != cur) {
            actual[i++] = cur.getFirst().getValue();
            cur = cur.getRest();
        }

        assertArrayEquals(expected, actual);
    }

    private int countNotes(final Atom a) {
        AtomList cur = a.getNotes();
        int count = 0;
        while (cur != null) {
            count++;
            cur = cur.getRest();
        }

        return count;
    }

    private List<Atom> getNotes(Atom a) {
        List<Atom> l = new LinkedList<Atom>();
        AtomList cur = a.getNotes();
        while (null != cur) {
            l.add(cur.getFirst());
            cur = cur.getRest();
        }

        return l;
    }

    private int countVertices() {
        int count = 0;
        for (Vertex v : graph.getVertices()) {
            count++;
        }
        return count;
    }

    private int countEdges() {
        int count = 0;
        for (Edge v : graph.getEdges()) {
            count++;
        }
        return count;
    }

    private Atom createAtom(final String key) {
        Atom a = manager.frame(graph.addVertex(key), Atom.class);
        a.setWeight(0.5f);
        a.setSharability(0.5f);
        return a;
    }

    //public void main(final String[] args) throws Exception {
    @Test
    public void testTmp() throws Exception {
        /*
        Graph g = new Neo4jGraph("/Users/josh/data/tinkernotes");
        GraphMLWriter w = new GraphMLWriter(g);
        w.setNormalize(true);
        OutputStream out = new FileOutputStream("/tmp/tinkernotes.xml");
        try {
            w.outputGraph(out);
        } finally {
            out.close();
        } //*/

        /*
        IndexableGraph g = new Neo4jGraph("/Users/josh/data/tinkernotes");
        Set<String> keys = new HashSet<String>();
        keys.add("key");
        if (null == g.getIndex("keys", Vertex.class)) {
            g.createAutomaticIndex("keys", Vertex.class, keys);
        }

        GraphMLReader r = new GraphMLReader(g);
        InputStream in = new FileInputStream("/tmp/tinkernotes.xml");
        try {
            r.inputGraph(in);
        } finally {
            in.close();
        }
        //*/
    }
}
