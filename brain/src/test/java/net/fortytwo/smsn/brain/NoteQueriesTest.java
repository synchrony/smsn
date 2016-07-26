package net.fortytwo.smsn.brain;

import com.tinkerpop.blueprints.impls.tg.TinkerGraph;
import net.fortytwo.smsn.brain.model.Atom;
import net.fortytwo.smsn.brain.model.AtomGraph;
import net.fortytwo.smsn.brain.model.AtomList;
import net.fortytwo.smsn.brain.model.Filter;
import net.fortytwo.smsn.brain.model.Note;
import net.fortytwo.smsn.brain.model.pg.PGAtomGraph;
import net.fortytwo.smsn.brain.wiki.NoteParser;
import net.fortytwo.smsn.brain.wiki.NoteWriter;
import org.json.JSONObject;
import org.junit.Assert;
import org.junit.Before;
import org.junit.Test;

import java.util.List;

import static junit.framework.Assert.assertEquals;
import static junit.framework.Assert.assertFalse;
import static junit.framework.Assert.assertTrue;
import static org.junit.Assert.assertArrayEquals;
import static org.junit.Assert.assertNotNull;
import static org.junit.Assert.assertNull;

/**
 * @author Joshua Shinavier (http://fortytwo.net)
 */
public class NoteQueriesTest {
    private AtomGraph atomGraph;
    private NoteParser parser;
    private final NoteWriter writer = new NoteWriter();
    private NoteQueries queries;
    private Filter filter;

    @Before
    public void setUp() throws Exception {
        TinkerGraph g = new TinkerGraph();
        parser = new NoteParser();
        atomGraph = new PGAtomGraph(g);
        Brain brain = new Brain(atomGraph);
        queries = new NoteQueries(brain);
        filter = new Filter();


    }

    @Test
    public void testEncoding() throws Exception {
        NoteQueries.ViewStyle style = NoteQueries.forwardViewStyle;

        Atom root = createAtom("11111");
        assertEquals("11111", root.getId());

        //Note superRoot = new Note();
        Note rootNote = new Note();
        rootNote.setId(root.getId());
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
        NoteQueries.ViewStyle style = NoteQueries.forwardViewStyle;
        Atom root = createAtom("wXu5g4v");
        root.setValue("root");
        Note rootNote;
        String s;

        s = "" +
                "* :N5KBOAq: one\n" +
                "* :v8EuMtl: two\n" +
                "* :tOpwKho: three\n";
        rootNote = parser.fromWikiText(s);
        rootNote.setId(root.getId());
        queries.update(rootNote, 2, filter, style);
        assertNotesEqual(root, "one", "two", "three");

        Atom one = atomGraph.getAtom("N5KBOAq");
        Atom two = atomGraph.getAtom("v8EuMtl");

        s = "" +
                "* :N5KBOAq: one\n" +
                "    * :r4zU45R: ten\n" +
                "    * yellow\n" +
                "* :tOpwKho: three\n";
        rootNote = parser.fromWikiText(s);
        rootNote.setId(root.getId());
        queries.update(rootNote, 2, filter, style);
        // "two" has been removed
        assertNotesEqual(root, "one", "three");
        // grandchildren have been added
        assertNotesEqual(one, "ten", "yellow");
        Atom ten = atomGraph.getAtom("r4zU45R");

        s = "" +
                "* :N5KBOAq: one\n" +
                "    * :r4zU45R: ten\n" +
                "        * rabbit\n" +
                "    * purple\n" +
                "* :tOpwKho: three\n";
        rootNote = parser.fromWikiText(s);
        rootNote.setId(root.getId());
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
        rootNote.setId(root.getId());
        queries.update(rootNote, 2, filter, style);
        Atom green = one.getNotes().getRest().getFirst();
        // "rabbit" and "kangaroo" are added beneath "green" even though they're
        // deeper than 2 steps in the tree, because "green" is a new note
        assertNotesEqual(green, "rabbit", "kangaroo");

        s = "" +
                "* :v8EuMtl: two\n" +
                "* :tOpwKho: three\n";
        rootNote = parser.fromWikiText(s);
        rootNote.setId(root.getId());
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
        rootNote.setId(root.getId());
        queries.update(rootNote, 2, filter, style);
        // we swapped the order of "two" and "three"...
        assertNotesEqual(root, "three", "two");
        Atom three = atomGraph.getAtom("tOpwKho");
        // ...therefore, the children of "three" can't be modified in this update operation
        // (so "red" has been ignored)
        assertNotesEqual(three);

        s = "" +
                "* :v8EuMtl: two\n" +
                "    * elephant\n" +
                "* :v8EuMtl: two\n" +
                "* :tOpwKho: three\n";
        rootNote = parser.fromWikiText(s);
        rootNote.setId(root.getId());
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
        rootNote.setId(root.getId());
        queries.update(rootNote, 2, filter, style);
        assertNotesEqual(root, "two", "two", "three");
        // when duplicates already exist, children of duplicates follow the last-occurring instance
        assertNotesEqual(two, "gorilla");
    }

    @Test
    public void testPathologicalUpdateWithCycles() throws Exception {
        NoteQueries.ViewStyle style = NoteQueries.forwardViewStyle;
        Atom root = createAtom("0000000");
        root.setValue("root");
        Note rootNote, child, grandChild;
        String s;

        // OK to create an atom which is its own parent
        s = "" +
                "* :001: one\n" +
                "    * :001: one\n";
        rootNote = parser.fromWikiText(s);
        rootNote.setId(root.getId());
        queries.update(rootNote, 2, filter, style);
        Atom one = atomGraph.getAtom("001");
        assertNotNull(one);
        Assert.assertEquals(1, one.getNotes().toJavaList().size());
        for (int i = 0; i < 2; i++) {
            assertEquals(1, rootNote.getChildren().size());
            child = rootNote.getChildren().get(0);
            Assert.assertEquals("001", child.getId());
            assertEquals(1, child.getChildren().size());
            grandChild = child.getChildren().get(0);
            assertEquals("001", grandChild.getId());

            rootNote = queries.view(root, 2, filter, style);
        }

        // setting properties at the higher level has no effect, as we are pre-ordered w.r.t. updating of properties
        s = "" +
                "* :001: one - updated\n" +
                "    * :001: one\n";
        rootNote = parser.fromWikiText(s);
        rootNote.setId(root.getId());
        queries.update(rootNote, 2, filter, style);
        one = atomGraph.getAtom("001");
        assertNotNull(one);
        Assert.assertEquals(1, one.getNotes().toJavaList().size());
        rootNote = queries.view(root, 2, filter, style);
        assertEquals(1, rootNote.getChildren().size());
        child = rootNote.getChildren().get(0);
        Assert.assertEquals("001", child.getId());
        Assert.assertEquals("one", child.getValue());
        assertEquals(1, child.getChildren().size());
        grandChild = child.getChildren().get(0);
        assertEquals("001", grandChild.getId());

        // setting properties at the lower level (the last visited) does have an effect
        s = "" +
                "* :001: one\n" +
                "    * :001: one - updated\n";
        rootNote = parser.fromWikiText(s);
        rootNote.setId(root.getId());
        queries.update(rootNote, 2, filter, style);
        one = atomGraph.getAtom("001");
        assertNotNull(one);
        Assert.assertEquals(1, one.getNotes().toJavaList().size());
        rootNote = queries.view(root, 2, filter, style);
        assertEquals(1, rootNote.getChildren().size());
        child = rootNote.getChildren().get(0);
        Assert.assertEquals("001", child.getId());
        Assert.assertEquals("one - updated", child.getValue());
        assertEquals(1, child.getChildren().size());
        grandChild = child.getChildren().get(0);
        assertEquals("001", grandChild.getId());

        // the preorder rule does not apply when the link from parent to child is not repeated in the view;
        // the children of an atom are updated only once
        s = "" +
                "* :001: one\n" +
                "    * :001: one\n" +
                "    * :002: two\n";
        rootNote = parser.fromWikiText(s);
        rootNote.setId(root.getId());
        queries.update(rootNote, 2, filter, style);
        one = atomGraph.getAtom("001");
        assertNotNull(one);
        Assert.assertEquals(2, one.getNotes().toJavaList().size());
        for (int i = 0; i < 2; i++) {
            assertEquals(1, rootNote.getChildren().size());
            child = rootNote.getChildren().get(0);
            Assert.assertEquals("001", child.getId());
            assertEquals(2, child.getChildren().size());
            grandChild = child.getChildren().get(0);
            assertEquals("001", grandChild.getId());
            grandChild = child.getChildren().get(1);
            assertEquals("002", grandChild.getId());

            rootNote = queries.view(root, 2, filter, style);
        }

        // get the height-3 view
        rootNote = queries.view(root, 3, filter, style);
        assertEquals(1, rootNote.getChildren().size());
        child = rootNote.getChildren().get(0);
        Assert.assertEquals("001", child.getId());
        assertEquals(2, child.getChildren().size());
        grandChild = child.getChildren().get(0);
        assertEquals("001", grandChild.getId());
        Assert.assertEquals(2, grandChild.getChildren().size());
        grandChild = child.getChildren().get(1);
        assertEquals("002", grandChild.getId());
        Assert.assertEquals(0, grandChild.getChildren().size());

        // adding or removing at the higher level has no effect, as we are pre-ordered w.r.t. updating of children
        s = "" +
                "* :001: one\n" +
                "    * :001: one\n" +
                "        * :001: one\n" +
                "        * :002: two\n" +
                "    * :002: two\n" +
                "    * :003: three\n";
        rootNote = parser.fromWikiText(s);
        rootNote.setId(root.getId());
        queries.update(rootNote, 3, filter, style);
        rootNote = queries.view(root, 3, filter, style);
        assertEquals(1, rootNote.getChildren().size());
        child = rootNote.getChildren().get(0);
        Assert.assertEquals("001", child.getId());
        assertEquals(2, child.getChildren().size());
        grandChild = child.getChildren().get(0);
        assertEquals("001", grandChild.getId());
        Assert.assertEquals(2, grandChild.getChildren().size());
        grandChild = child.getChildren().get(1);
        assertEquals("002", grandChild.getId());
        Assert.assertEquals(0, grandChild.getChildren().size());

        // adding or removing children at the lower level (the last visited) does have an effect
        s = "" +
                "* :001: one\n" +
                "    * :001: one\n" +
                "        * :001: one\n" +
                "        * :002: two\n" +
                "        * :003: three\n" +
                "    * :002: two\n";
        rootNote = parser.fromWikiText(s);
        rootNote.setId(root.getId());
        queries.update(rootNote, 3, filter, style);
        rootNote = queries.view(root, 3, filter, style);
        assertEquals(1, rootNote.getChildren().size());
        child = rootNote.getChildren().get(0);
        Assert.assertEquals("001", child.getId());
        assertEquals(3, child.getChildren().size());
        grandChild = child.getChildren().get(0);
        assertEquals("001", grandChild.getId());
        Assert.assertEquals(3, grandChild.getChildren().size());
        grandChild = child.getChildren().get(1);
        assertEquals("002", grandChild.getId());
        Assert.assertEquals(0, grandChild.getChildren().size());
    }

    @Test
    public void testUpdateSharabilityOrWeight() throws Exception {
        NoteQueries.ViewStyle style = NoteQueries.forwardViewStyle;
        Atom root = createAtom("wXu5g4v");
        root.setValue("root");
        Note rootNote;
        String s;

        s = "" +
                "* :N5KBOAq: one\n";
        rootNote = parser.fromWikiText(s);
        rootNote.setId(root.getId());
        queries.update(rootNote, 2, filter, style);
        Atom one = atomGraph.getAtom("N5KBOAq");
        assertEquals(0.5f, one.getWeight());
        assertEquals(0.5f, one.getSharability());

        s = "" +
                "* :N5KBOAq: one\n" +
                "    @weight 0.75\n" +
                "    @sharability 0.25\n";
        rootNote = parser.fromWikiText(s);
        rootNote.setId(root.getId());
        queries.update(rootNote, 2, filter, style);
        assertEquals(0.75f, one.getWeight());
        assertEquals(0.25f, one.getSharability());
    }

    @Test
    public void testUpdateAlias() throws Exception {
        NoteQueries.ViewStyle style = NoteQueries.forwardViewStyle;
        Atom root = createAtom("wXu5g4v");
        root.setValue("root");
        Note rootNote;
        String s;

        s = "" +
                "* :N5KBOAq: one\n" +
                "    @alias http://example.org/ns/one\n";
        rootNote = parser.fromWikiText(s);
        rootNote.setId(root.getId());
        queries.update(rootNote, 2, filter, style);
        Atom one = atomGraph.getAtom("N5KBOAq");
        assertEquals("http://example.org/ns/one", one.getAlias());

        s = "" +
                "* :N5KBOAq: one\n" +
                "    @alias \n";
        rootNote = parser.fromWikiText(s);
        rootNote.setId(root.getId());
        queries.update(rootNote, 2, filter, style);
        assertNull(one.getAlias());
    }

    @Test
    public void testUpdatePriority() throws Exception {
        NoteQueries.ViewStyle style = NoteQueries.forwardViewStyle;
        Atom root = createAtom("0000000");
        root.setValue("root");
        Note rootNote;
        String s;
        Atom one;

        s = "" +
                "* :0000001: one\n" +
                "    @priority 0.5";
        rootNote = parser.fromWikiText(s);
        rootNote.setId(root.getId());
        queries.update(rootNote, 2, filter, style);
        one = atomGraph.getAtom("0000001");
        assertEquals(0.5f, one.getPriority());

        // setting priority to 0 has the effect of removing the priority property from the note
        s = "" +
                "* :0000001: one\n" +
                "    @priority 0";
        rootNote = parser.fromWikiText(s);
        rootNote.setId(root.getId());
        queries.update(rootNote, 2, filter, style);
        one = atomGraph.getAtom("0000001");
        assertNull(one.getPriority());
    }

    // TODO: test write behavior w.r.t. sharability filters
    @Test
    public void testHideNonSharableItems() throws Exception {
        Filter readFilter = new Filter(0f, 1f, 0.5f, 0.75f, 1f, 0.75f);
        Filter writeFilter = new Filter(0f, 1f, 0.5f, 0f, 1f, 0.5f);
        NoteQueries.ViewStyle style = NoteQueries.forwardViewStyle;

        Note rootNote = parser.fromWikiText(NoteParser.class.getResourceAsStream("wiki-example-3.txt"));
        Atom root = createAtom("0000000");
        root.setValue("root");
        root.setSharability(1.0f);
        rootNote.setId(root.getId());
        queries.update(rootNote, 2, writeFilter, style);

        Atom a1 = atomGraph.getAtom("0000001");
        assertEquals(1f, a1.getSharability());
        Atom a2 = atomGraph.getAtom("0000002");
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

        Atom root = atomGraph.createAtom(filter, "000");
        root.setValue("root");

        b.setId(root.getId());
        queries.update(b, 2, filter, style);

        Atom a1 = atomGraph.getAtom("001");
        Atom a2 = atomGraph.getAtom("002");
        Atom a3 = atomGraph.getAtom("003");

        assertEquals("one", a1.getValue());
        assertEquals("two", a2.getValue());
        assertEquals("three", a3.getValue());

        a.setId(root.getId());
        queries.update(a, 2, filter, style);

        // 002's value was unaffected by the update
        assertEquals("ONE", a1.getValue());
        assertEquals("two", a2.getValue());
        assertEquals("THREE", a3.getValue());
    }

    @Test
    public void testAddOnlyUpdate() throws Exception {
        NoteQueries.ViewStyle style = NoteQueries.forwardAddOnlyViewStyle;

        String before = "* :001: one\n" +
                "* :002: two\n" +
                "* :003: three";
        String after = "* :004: four\n" +
                "* :002: two";

        Note b = parser.fromWikiText(before);
        Note a = parser.fromWikiText(after);

        Atom root = atomGraph.createAtom(filter, "000");
        root.setValue("root");

        b.setId(root.getId());
        queries.update(b, 2, filter, style);

        Atom a1 = atomGraph.getAtom("001");
        Atom a2 = atomGraph.getAtom("002");
        Atom a3 = atomGraph.getAtom("003");

        assertEquals("one", a1.getValue());
        assertEquals("two", a2.getValue());
        assertEquals("three", a3.getValue());
        assertEquals(3, root.getNotes().toJavaList().size());

        a.setId(root.getId());
        queries.update(a, 2, filter, style);

        Atom a4 = atomGraph.getAtom("004");

        assertEquals("four", a4.getValue());
        List<Atom> children = root.getNotes().toJavaList();
        assertEquals(4, children.size());
        assertEquals("four", children.get(0).getValue());
        assertEquals("one", children.get(1).getValue());
        assertEquals("two", children.get(2).getValue());
    }

    @Test
    public void testFindRootsAndIsolatedAtoms() throws Exception {
        assertEquals(0, queries.findRootAtoms(filter, NoteQueries.forwardViewStyle, 1).getChildren().size());
        assertEquals(0, queries.findRootAtoms(filter, NoteQueries.backwardViewStyle, 1).getChildren().size());
        assertEquals(0, queries.findIsolatedAtoms(filter).getChildren().size());

        Atom atom0 = atomGraph.createAtom(filter, "000");
        atom0.setValue("0");

        assertEquals(1, queries.findRootAtoms(filter, NoteQueries.forwardViewStyle, 1).getChildren().size());
        assertEquals(1, queries.findRootAtoms(filter, NoteQueries.backwardViewStyle, 1).getChildren().size());
        assertEquals(1, queries.findIsolatedAtoms(filter).getChildren().size());

        Atom atom1 = atomGraph.createAtom(filter, "001");
        atom1.setValue("1");

        assertEquals(2, queries.findRootAtoms(filter, NoteQueries.forwardViewStyle, 1).getChildren().size());
        assertEquals(2, queries.findRootAtoms(filter, NoteQueries.backwardViewStyle, 1).getChildren().size());
        assertEquals(2, queries.findIsolatedAtoms(filter).getChildren().size());

        atom0.addChildAt(atom1, 0);

        assertEquals(1, queries.findRootAtoms(filter, NoteQueries.forwardViewStyle, 1).getChildren().size());
        assertEquals(1, queries.findRootAtoms(filter, NoteQueries.backwardViewStyle, 1).getChildren().size());
        assertEquals(0, queries.findIsolatedAtoms(filter).getChildren().size());

        Atom atom2 = atomGraph.createAtom(filter, "002");
        atom2.setValue("2");

        assertEquals(2, queries.findRootAtoms(filter, NoteQueries.forwardViewStyle, 1).getChildren().size());
        assertEquals(2, queries.findRootAtoms(filter, NoteQueries.backwardViewStyle, 1).getChildren().size());
        assertEquals(1, queries.findIsolatedAtoms(filter).getChildren().size());

        atom0.addChildAt(atom2, 0);

        assertEquals(1, queries.findRootAtoms(filter, NoteQueries.forwardViewStyle, 1).getChildren().size());
        assertEquals(2, queries.findRootAtoms(filter, NoteQueries.backwardViewStyle, 1).getChildren().size());
        assertEquals(0, queries.findIsolatedAtoms(filter).getChildren().size());
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

    private Atom createAtom(String id) {
        return atomGraph.createAtom(filter, id);
    }
}
