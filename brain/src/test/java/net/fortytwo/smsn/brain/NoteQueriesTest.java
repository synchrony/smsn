package net.fortytwo.smsn.brain;

import com.tinkerpop.blueprints.Edge;
import com.tinkerpop.blueprints.KeyIndexableGraph;
import com.tinkerpop.blueprints.Vertex;
import com.tinkerpop.blueprints.impls.tg.TinkerGraph;
import com.tinkerpop.frames.FramedGraph;
import net.fortytwo.smsn.brain.wiki.NoteParser;
import net.fortytwo.smsn.brain.wiki.NoteWriter;
import org.json.JSONObject;
import org.junit.After;
import org.junit.Before;
import org.junit.Test;

import java.util.LinkedList;
import java.util.List;

import static junit.framework.Assert.assertEquals;
import static junit.framework.Assert.assertFalse;
import static junit.framework.Assert.assertTrue;
import static org.junit.Assert.assertArrayEquals;
import static org.junit.Assert.assertNull;

/**
 * @author Joshua Shinavier (http://fortytwo.net)
 */
public class NoteQueriesTest {
    private KeyIndexableGraph graph;
    private BrainGraph store;
    private FramedGraph<KeyIndexableGraph> manager;
    private NoteParser parser;
    private NoteWriter writer = new NoteWriter();
    private NoteQueries queries;

    @Before
    public void setUp() throws Exception {
        TinkerGraph g = new TinkerGraph();
        parser = new NoteParser();
        store = new BrainGraph(g);
        graph = store.getPropertyGraph();
        manager = store.getFramedGraph();
        ExtendoBrain brain = new ExtendoBrain(store);
        queries = new NoteQueries(brain);
    }

    @After
    public void tearDown() throws Exception {
        graph.shutdown();
    }

    @Test
    public void testEncoding() throws Exception {
        Filter filter = new Filter(0f, 1f, 0.5f, 0f, 1f, 0.5f);
        NoteQueries.ViewStyle style = NoteQueries.forwardViewStyle;

        Atom root = createAtom("11111");
        assertEquals("11111", root.asVertex().getId());

        //Note superRoot = new Note();
        Note rootNote = new Note();
        rootNote.setId((String) root.asVertex().getId());
        //superRoot.addChild(rootNote);
        rootNote.setValue("foo");
        Note child = new Note();
        child.setValue("cheval \u00e0 phynances");
        rootNote.addChild(child);
        assertNull(child.getWeight());
        assertNull(child.getSharability());
        assertNull(child.getCreated());
        //System.out.println(before.getTargetValue());

        queries.update(rootNote, 1, filter, style);

        //new GraphMLWriter(graph).outputGraph(System.out);

        Note after = queries.view(root, 1, filter, style);

        assertEquals("11111", after.getId());
        assertEquals("foo", after.getValue());
        assertEquals(1, after.getChildren().size());

        JSONObject json = writer.toJSON(after);
        //System.out.println(json.toString());
        JSONObject j = json.getJSONArray("children").getJSONObject(0);
        assertEquals("cheval \u00e0 phynances", j.getString("value"));
    }

    @Test
    public void testUpdateRecursion() throws Exception {
        Filter filter = new Filter(0f, 1f, 0.5f, 0f, 1f, 0.5f);
        NoteQueries.ViewStyle style = NoteQueries.forwardViewStyle;
        Atom root = createAtom("wXu5g4v");
        Note rootNote;
        String s;

        s = "" +
                "* :N5KBOAq: one\n" +
                "* :v8EuMtl: two\n" +
                "* :tOpwKho: three\n";
        rootNote = parser.fromWikiText(s);
        rootNote.setId((String) root.asVertex().getId());
        queries.update(rootNote, 2, filter, style);
        assertNotesEqual(root, "one", "two", "three");

        Atom one = store.getAtom("N5KBOAq");
        Atom two = store.getAtom("v8EuMtl");

        s = "" +
                "* :N5KBOAq: one\n" +
                "    * :r4zU45R: ten\n" +
                "    * yellow\n" +
                "* :tOpwKho: three\n";
        rootNote = parser.fromWikiText(s);
        rootNote.setId((String) root.asVertex().getId());
        queries.update(rootNote, 2, filter, style);
        // "two" has been removed
        assertNotesEqual(root, "one", "three");
        // grandchildren have been added
        assertNotesEqual(one, "ten", "yellow");
        Atom ten = store.getAtom("r4zU45R");

        s = "" +
                "* :N5KBOAq: one\n" +
                "    * :r4zU45R: ten\n" +
                "        * rabbit\n" +
                "    * purple\n" +
                "* :tOpwKho: three\n";
        rootNote = parser.fromWikiText(s);
        rootNote.setId((String) root.asVertex().getId());
        queries.update(rootNote, 2, filter, style);
        // depth is only two, so "rabbit" is not reachable
        assertNotesEqual(ten);

        s = "" +
                "* :N5KBOAq: one\n" +
                "    * :r4zU45R: ten\n" +
                "    * green\n" +
                "        * rabbit\n" +
                "        * kangaroo\n" +
                "* :tOpwKho: three\n";
        rootNote = parser.fromWikiText(s);
        rootNote.setId((String) root.asVertex().getId());
        queries.update(rootNote, 2, filter, style);
        Atom green = one.getNotes().getRest().getFirst();
        // "rabbit" and "kangaroo" are added beneath "green" even though they're
        // deeper than 2 steps in the tree, because "green" is a new note
        assertNotesEqual(green, "rabbit", "kangaroo");

        s = "" +
                "* :v8EuMtl: two\n" +
                "* :tOpwKho: three\n";
        rootNote = parser.fromWikiText(s);
        rootNote.setId((String) root.asVertex().getId());
        queries.update(rootNote, 2, filter, style);
        // "one" has been removed...
        assertNotesEqual(root, "two", "three");
        // but "one" still exists and has its previous notes
        assertNotesEqual(one, "ten", "green");

        s = "" +
                "* :tOpwKho: three\n" +
                "    * red\n" +
                "* :v8EuMtl: two\n";
        rootNote = parser.fromWikiText(s);
        rootNote.setId((String) root.asVertex().getId());
        queries.update(rootNote, 2, filter, style);
                // we swapped the order of "two" and "three"...
        assertNotesEqual(root, "three", "two");
        Atom three = store.getAtom("tOpwKho");
        // ...therefore, the children of "three" can't be modified in this update operation
        // (so "red" has been ignored)
        assertNotesEqual(three);

        s = "" +
                "* :v8EuMtl: two\n" +
                "    * elephant\n" +
                "* :v8EuMtl: two\n" +
                "* :tOpwKho: three\n";
        rootNote = parser.fromWikiText(s);
        rootNote.setId((String) root.asVertex().getId());
        queries.update(rootNote, 2, filter, style);
                // duplicates are possible...
        assertNotesEqual(root, "two", "two", "three");
        // ...but when a duplicate is added, children of any matching duplicate will be ignored
        assertNotesEqual(two);

        s = "" +
                "* :v8EuMtl: two\n" +
                "    * elephant\n" +
                "* :v8EuMtl: two\n" +
                "    * gorilla\n" +
                "* :tOpwKho: three\n";
        rootNote = parser.fromWikiText(s);
        rootNote.setId((String) root.asVertex().getId());
        queries.update(rootNote, 2, filter, style);
        assertNotesEqual(root, "two", "two", "three");
        // when duplicates already exist, children of duplicates follow the last-occurring instance
        assertNotesEqual(two, "gorilla");
    }

    @Test
    public void testUpdateSharabilityOrWeight() throws Exception {
        Filter filter = new Filter(0f, 1f, 0.5f, 0f, 1f, 0.5f);
        NoteQueries.ViewStyle style = NoteQueries.forwardViewStyle;
        Atom root = createAtom("wXu5g4v");
        Note rootNote;
        String s;

        s = "" +
                "* :N5KBOAq: one\n";
        rootNote = parser.fromWikiText(s);
        rootNote.setId((String) root.asVertex().getId());
        queries.update(rootNote, 2, filter, style);
        Atom one = store.getAtom("N5KBOAq");
        assertEquals(0.5f, one.getWeight());
        assertEquals(0.5f, one.getSharability());

        s = "" +
                "* :N5KBOAq: one\n" +
                "    @weight 0.75\n" +
                "    @sharability 0.25\n";
        rootNote = parser.fromWikiText(s);
        rootNote.setId((String) root.asVertex().getId());
        queries.update(rootNote, 2, filter, style);
        assertEquals(0.75f, one.getWeight());
        assertEquals(0.25f, one.getSharability());
    }

    @Test
    public void testUpdateAlias() throws Exception {
        Filter filter = new Filter(0f, 1f, 0.5f, 0f, 1f, 0.5f);
        NoteQueries.ViewStyle style = NoteQueries.forwardViewStyle;
        Atom root = createAtom("wXu5g4v");
        Note rootNote;
        String s;

        s = "" +
                "* :N5KBOAq: one\n" +
                "    @alias http://example.org/ns/one\n";
        rootNote = parser.fromWikiText(s);
        rootNote.setId((String) root.asVertex().getId());
        queries.update(rootNote, 2, filter, style);
        Atom one = store.getAtom("N5KBOAq");
        assertEquals("http://example.org/ns/one", one.getAlias());

        s = "" +
                "* :N5KBOAq: one\n" +
                "    @alias \n";
        rootNote = parser.fromWikiText(s);
        rootNote.setId((String) root.asVertex().getId());
        queries.update(rootNote, 2, filter, style);
        assertNull(one.getAlias());
    }

    @Test
    public void testUpdatePriority() throws Exception {
        Filter filter = new Filter(0f, 1f, 0.5f, 0f, 1f, 0.5f);
        NoteQueries.ViewStyle style = NoteQueries.forwardViewStyle;
        Atom root = createAtom("priorit");
        Note rootNote;
        String s;
        Atom one;

        s = "" +
                "* :0000001: one\n" +
                "    @priority 0.5";
        rootNote = parser.fromWikiText(s);
        rootNote.setId((String) root.asVertex().getId());
        queries.update(rootNote, 2, filter, style);
        one = store.getAtom("0000001");
        assertEquals(0.5f, one.getPriority());

        // setting priority to 0 has the effect of removing the priority property from the note
        s = "" +
                "* :0000001: one\n" +
                "    @priority 0";
        rootNote = parser.fromWikiText(s);
        rootNote.setId((String) root.asVertex().getId());
        queries.update(rootNote, 2, filter, style);
        one = store.getAtom("0000001");
        assertNull(one.getPriority());
    }

    // TODO: test write behavior w.r.t. sharability filters
    @Test
    public void testHideNonSharableItems() throws Exception {
        Filter readFilter = new Filter(0f, 1f, 0.5f, 0.75f, 1f, 0.75f);
        Filter writeFilter = new Filter(0f, 1f, 0.5f, 0f, 1f, 0.5f);
        NoteQueries.ViewStyle style = NoteQueries.forwardViewStyle;

        Note rootNote = parser.fromWikiText(BrainGraph.class.getResourceAsStream("wiki-example-3.txt"));
        Atom root = createAtom("0000000");
        root.setSharability(1.0f);
        rootNote.setId((String) root.asVertex().getId());
        queries.update(rootNote, 2, writeFilter, style);

        Atom a1 = store.getAtom("0000001");
        assertEquals(1f, a1.getSharability());
        Atom a2 = store.getAtom("0000002");
        assertEquals(0.5f, a2.getSharability());

        Note after = queries.view(root, 2, readFilter, style);
        //writer.writeNotes(after.getChildren(), System.out);
        assertTrue(after.getHasChildren());
        List<Note> children = after.getChildren();
        Note n1 = children.get(0);
        assertTrue(n1.getHasChildren());

        // This note is "invisible".
        Note n4 = children.get(1);
        // Its value appears as null.
        assertNull(n4.getValue());
        // This note has a child, but we can't see it.
        assertEquals(0, n4.getChildren().size());
        // We can't even see whether the node has children or not.
        assertFalse(n4.getHasChildren());

        Note n5 = children.get(2);
        assertFalse(n5.getHasChildren());

        List<Note> grandChildren = n1.getChildren();
        assertEquals(2, grandChildren.size());
        Note n2 = grandChildren.get(0);
        assertFalse(n2.getHasChildren());
        Note n3 = grandChildren.get(1);
        assertFalse(n3.getHasChildren());
        assertNull(n2.getValue());
        assertEquals("this is a public child of a public note", n3.getValue());
    }

    @Test
    public void testDontOverwriteNotesWithEmptyValues() throws Exception {
        Filter filter = new Filter(0f, 1f, 0.5f, 0f, 1f, 0.5f);
        NoteQueries.ViewStyle style = NoteQueries.forwardViewStyle;

        String before = "* :001: one\n" +
                "* :002: two\n" +
                "* :003: three";
        String after = "* :001: ONE\n" +
                "* :002:\n" +
                "* :003: THREE";

        Note b = parser.fromWikiText(before);
        Note a = parser.fromWikiText(after);

        // First, check that 'after' was parsed correctly
        assertEquals(3, a.getChildren().size());
        assertEquals("002", a.getChildren().get(1).getId());
        assertNull(a.getChildren().get(1).getValue());

        Atom root = store.createAtom(filter, "000");

        b.setId((String) root.asVertex().getId());
        queries.update(b, 2, filter, style);

        Atom a1 = store.getAtom("001");
        Atom a2 = store.getAtom("002");
        Atom a3 = store.getAtom("003");

        assertEquals("one", a1.getValue());
        assertEquals("two", a2.getValue());
        assertEquals("three", a3.getValue());

        a.setId((String) root.asVertex().getId());
        queries.update(a, 2, filter, style);

        // 002's value was unaffected by the update
        assertEquals("ONE", a1.getValue());
        assertEquals("two", a2.getValue());
        assertEquals("THREE", a3.getValue());
    }

    @Test
    public void testAddOnlyUpdate() throws Exception {
        Filter filter = new Filter(0f, 1f, 0.5f, 0f, 1f, 0.5f);
        NoteQueries.ViewStyle style = NoteQueries.forwardAddOnlyViewStyle;

        String before = "* :001: one\n" +
                "* :002: two\n" +
                "* :003: three";
        String after = "* :004: four\n" +
                "* :002: two";

        Note b = parser.fromWikiText(before);
        Note a = parser.fromWikiText(after);

        Atom root = store.createAtom(filter, "000");

        b.setId((String) root.asVertex().getId());
        queries.update(b, 2, filter, style);

        Atom a1 = store.getAtom("001");
        Atom a2 = store.getAtom("002");
        Atom a3 = store.getAtom("003");

        assertEquals("one", a1.getValue());
        assertEquals("two", a2.getValue());
        assertEquals("three", a3.getValue());
        assertEquals(3, BrainGraph.asList(root.getNotes()).size());

        a.setId((String) root.asVertex().getId());
        queries.update(a, 2, filter, style);

        Atom a4 = store.getAtom("004");

        assertEquals("four", a4.getValue());
        List<Atom> children = BrainGraph.asList(root.getNotes());
        assertEquals(4, children.size());
        assertEquals("four", children.get(0).getValue());
        assertEquals("one", children.get(1).getValue());
        assertEquals("two", children.get(2).getValue());
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
        List<Atom> l = new LinkedList<>();
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

    private Atom createAtom(final String id) {
        Atom a = manager.frame(graph.addVertex(id), Atom.class);
        a.setWeight(0.5f);
        a.setSharability(0.5f);
        return a;
    }
}
