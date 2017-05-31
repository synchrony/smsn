package net.fortytwo.smsn.brain;

import net.fortytwo.smsn.brain.error.InvalidUpdateException;
import net.fortytwo.smsn.brain.io.json.JsonPrinter;
import net.fortytwo.smsn.brain.model.Filter;
import net.fortytwo.smsn.brain.model.Note;
import net.fortytwo.smsn.brain.model.TopicGraph;
import net.fortytwo.smsn.brain.model.entities.Atom;
import net.fortytwo.smsn.brain.model.entities.EntityList;
import net.fortytwo.smsn.brain.query.ViewStyle;
import org.json.JSONObject;
import org.junit.Assert;
import org.junit.Ignore;
import org.junit.Test;

import java.io.IOException;
import java.util.List;
import java.util.Optional;

import static junit.framework.Assert.assertEquals;
import static org.junit.Assert.assertArrayEquals;
import static org.junit.Assert.assertNull;

public class TreeViewsTest extends BrainTestBase {
    private final JsonPrinter jsonPrinter = new JsonPrinter();

    @Override
    protected TopicGraph createAtomGraph() throws IOException {
        return createTinkerAtomGraph();
    }

    @Test
    public void testEncoding() throws Exception {
        ViewStyle style = ViewStyle.Basic.Forward.getStyle();

        Atom root = createAtom("11111", "root");
        assertEquals("11111", root.getId());

        //Note superRoot = new Note();
        Note rootNote = new Note();
        rootNote.setId(root.getId());
        //superRoot.addChild(rootNote);
        rootNote.setTitle("foo");
        Note child = new Note();
        child.setTitle("cheval \u00e0 phynances");
        rootNote.addChild(child);
        assertNull(child.getWeight());
        assertNull(child.getSource());
        assertNull(child.getCreated());
        //System.out.println(before.getTargetValue());

        queries.update(rootNote, 1, filter, style);

        //new GraphMLWriter(graph).outputGraph(System.out);

        Note after = queries.view(root, 1, filter, style);

        assertEquals("11111", after.getId());
        assertEquals("foo", after.getTitle());
        assertEquals(1, after.getChildren().size());

        JSONObject json = jsonPrinter.toJson(after);
        //System.out.println(json.toString());
        JSONObject j = json.getJSONArray("children").getJSONObject(0);
        assertEquals("cheval \u00e0 phynances", j.getString("title"));
    }

    @Ignore
    @Test(expected = InvalidUpdateException.class)
    public void updateWithPageAndChildrenIsRejected() throws Exception {
        Atom root = createAtom("11111", "root");

        Note note = new Note();
        note.setId(root.getId());
        note.setTitle("Arthur Dent");
        note.setPage("He's a jerk.\nA complete kneebiter.");
        Note child = new Note();
        child.setTitle("Random");
        note.addChild(child);

        queries.update(note, 5, Filter.noFilter(), ViewStyle.Basic.Forward.getStyle());
    }

    @Test
    public void updateWithPageAndChildrenIsAccepted() throws Exception {
        Atom root = createAtom("11111", "root");

        Note note = new Note();
        note.setId(root.getId());
        note.setTitle("Arthur Dent");
        note.setPage("He's a jerk.\nA complete kneebiter.");
        Note child = new Note();
        child.setTitle("Random");
        note.addChild(child);

        queries.update(note, 5, Filter.noFilter(), ViewStyle.Basic.Forward.getStyle());

        Atom ad = topicGraph.getAtomById(root.getId()).get();
        assertEquals("Arthur Dent", ad.getTitle());
        assertEquals("He's a jerk.\nA complete kneebiter.", ad.getText());
        assertEquals(1, EntityList.toJavaList(ad.getChildren()).size());
        Atom random = ad.getChildren().getFirst();
        assertEquals("Random", random.getTitle());
    }

    @Test
    public void testUpdateRecursion() throws Exception {
        ViewStyle style = ViewStyle.Basic.Forward.getStyle();
        Atom root = createAtom("wXu5g4v", "root");
        Note rootNote;
        String s;

        s = "" +
                "* :N5KBOAq: one\n" +
                "* :v8EuMtl: two\n" +
                "* :tOpwKho: three\n";
        rootNote = wikiParser.parse(s);
        rootNote.setId(root.getId());
        queries.update(rootNote, 2, filter, style);
        assertNotesEqual(root, "one", "two", "three");

        Atom one = topicGraph.getAtomById("N5KBOAq").get();
        Atom two = topicGraph.getAtomById("v8EuMtl").get();

        s = "" +
                "* :N5KBOAq: one\n" +
                "    * :r4zU45R: ten\n" +
                "    * yellow\n" +
                "* :tOpwKho: three\n";
        rootNote = wikiParser.parse(s);
        rootNote.setId(root.getId());
        queries.update(rootNote, 2, filter, style);
        // "two" has been removed
        assertNotesEqual(root, "one", "three");
        // grandchildren have been added
        assertNotesEqual(one, "ten", "yellow");
        Atom ten = topicGraph.getAtomById("r4zU45R").get();

        s = "" +
                "* :N5KBOAq: one\n" +
                "    * :r4zU45R: ten\n" +
                "        * rabbit\n" +
                "    * purple\n" +
                "* :tOpwKho: three\n";
        rootNote = wikiParser.parse(s);
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
        rootNote = wikiParser.parse(s);
        rootNote.setId(root.getId());
        queries.update(rootNote, 2, filter, style);
        Atom green = one.getChildren().getRest().getFirst();
        // "rabbit" and "kangaroo" are added beneath "green" even though they're
        // deeper than 2 steps in the tree, because "green" is a new note
        assertNotesEqual(green, "rabbit", "kangaroo");

        s = "" +
                "* :v8EuMtl: two\n" +
                "* :tOpwKho: three\n";
        rootNote = wikiParser.parse(s);
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
        rootNote = wikiParser.parse(s);
        rootNote.setId(root.getId());
        queries.update(rootNote, 2, filter, style);
        // we swapped the order of "two" and "three"...
        assertNotesEqual(root, "three", "two");
        Atom three = topicGraph.getAtomById("tOpwKho").get();
        // ...therefore, the children of "three" can't be modified in this update operation
        // (so "red" has been ignored)
        assertNotesEqual(three);

        s = "" +
                "* :v8EuMtl: two\n" +
                "    * elephant\n" +
                "* :v8EuMtl: two\n" +
                "* :tOpwKho: three\n";
        rootNote = wikiParser.parse(s);
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
        rootNote = wikiParser.parse(s);
        rootNote.setId(root.getId());
        queries.update(rootNote, 2, filter, style);
        assertNotesEqual(root, "two", "two", "three");
        // when duplicates already exist, children of duplicates follow the last-occurring instance
        assertNotesEqual(two, "gorilla");
    }

    @Test
    public void testPathologicalUpdateWithCycles() throws Exception {
        ViewStyle style = ViewStyle.Basic.Forward.getStyle();
        Atom root = createAtom("0000000", "root");
        Note rootNote, child, grandChild;
        String s;

        // OK to create an atom which is its own parent
        s = "" +
                "* :000001: one\n" +
                "    * :000001: one\n";
        rootNote = wikiParser.parse(s);
        rootNote.setId(root.getId());
        queries.update(rootNote, 2, filter, style);
        Atom one = topicGraph.getAtomById("000001").get();
        Assert.assertEquals(1, EntityList.toJavaList(one.getChildren()).size());
        for (int i = 0; i < 2; i++) {
            assertEquals(1, rootNote.getChildren().size());
            child = rootNote.getChildren().get(0);
            Assert.assertEquals("000001", child.getId());
            assertEquals(1, child.getChildren().size());
            grandChild = child.getChildren().get(0);
            assertEquals("000001", grandChild.getId());

            rootNote = queries.view(root, 2, filter, style);
        }

        // setting properties at the higher level has no effect, as we are pre-ordered w.r.t. updating of properties
        s = "" +
                "* :000001: one - updated\n" +
                "    * :000001: one\n";
        rootNote = wikiParser.parse(s);
        rootNote.setId(root.getId());
        queries.update(rootNote, 2, filter, style);
        one = topicGraph.getAtomById("000001").get();
        Assert.assertEquals(1, EntityList.toJavaList(one.getChildren()).size());
        rootNote = queries.view(root, 2, filter, style);
        assertEquals(1, rootNote.getChildren().size());
        child = rootNote.getChildren().get(0);
        Assert.assertEquals("000001", child.getId());
        Assert.assertEquals("one", child.getTitle());
        assertEquals(1, child.getChildren().size());
        grandChild = child.getChildren().get(0);
        assertEquals("000001", grandChild.getId());

        // setting properties at the lower level (the last visited) does have an effect
        s = "" +
                "* :000001: one\n" +
                "    * :000001: one - updated\n";
        rootNote = wikiParser.parse(s);
        rootNote.setId(root.getId());
        queries.update(rootNote, 2, filter, style);
        one = topicGraph.getAtomById("000001").get();
        Assert.assertEquals(1, EntityList.toJavaList(one.getChildren()).size());
        rootNote = queries.view(root, 2, filter, style);
        assertEquals(1, rootNote.getChildren().size());
        child = rootNote.getChildren().get(0);
        Assert.assertEquals("000001", child.getId());
        Assert.assertEquals("one - updated", child.getTitle());
        assertEquals(1, child.getChildren().size());
        grandChild = child.getChildren().get(0);
        assertEquals("000001", grandChild.getId());

        // the preorder rule does not apply when the link from parent to child is not repeated in the view;
        // the children of an atom are updated only once
        s = "" +
                "* :000001: one\n" +
                "    * :000001: one\n" +
                "    * :000002: two\n";
        rootNote = wikiParser.parse(s);
        rootNote.setId(root.getId());
        queries.update(rootNote, 2, filter, style);
        one = topicGraph.getAtomById("000001").get();
        Assert.assertEquals(2, EntityList.toJavaList(one.getChildren()).size());
        for (int i = 0; i < 2; i++) {
            assertEquals(1, rootNote.getChildren().size());
            child = rootNote.getChildren().get(0);
            Assert.assertEquals("000001", child.getId());
            assertEquals(2, child.getChildren().size());
            grandChild = child.getChildren().get(0);
            assertEquals("000001", grandChild.getId());
            grandChild = child.getChildren().get(1);
            assertEquals("000002", grandChild.getId());

            rootNote = queries.view(root, 2, filter, style);
        }

        // get the height-3 view
        rootNote = queries.view(root, 3, filter, style);
        assertEquals(1, rootNote.getChildren().size());
        child = rootNote.getChildren().get(0);
        Assert.assertEquals("000001", child.getId());
        assertEquals(2, child.getChildren().size());
        grandChild = child.getChildren().get(0);
        assertEquals("000001", grandChild.getId());
        Assert.assertEquals(2, grandChild.getChildren().size());
        grandChild = child.getChildren().get(1);
        assertEquals("000002", grandChild.getId());
        Assert.assertEquals(0, grandChild.getChildren().size());

        // adding or removing at the higher level has no effect, as we are pre-ordered w.r.t. updating of children
        s = "" +
                "* :000001: one\n" +
                "    * :000001: one\n" +
                "        * :000001: one\n" +
                "        * :000002: two\n" +
                "    * :000002: two\n" +
                "    * :000003: three\n";
        rootNote = wikiParser.parse(s);
        rootNote.setId(root.getId());
        queries.update(rootNote, 3, filter, style);
        rootNote = queries.view(root, 3, filter, style);
        assertEquals(1, rootNote.getChildren().size());
        child = rootNote.getChildren().get(0);
        Assert.assertEquals("000001", child.getId());
        assertEquals(2, child.getChildren().size());
        grandChild = child.getChildren().get(0);
        assertEquals("000001", grandChild.getId());
        Assert.assertEquals(2, grandChild.getChildren().size());
        grandChild = child.getChildren().get(1);
        assertEquals("000002", grandChild.getId());
        Assert.assertEquals(0, grandChild.getChildren().size());

        // adding or removing children at the lower level (the last visited) does have an effect
        s = "" +
                "* :000001: one\n" +
                "    * :000001: one\n" +
                "        * :000001: one\n" +
                "        * :000002: two\n" +
                "        * :000003: three\n" +
                "    * :000002: two\n";
        rootNote = wikiParser.parse(s);
        rootNote.setId(root.getId());
        queries.update(rootNote, 3, filter, style);
        rootNote = queries.view(root, 3, filter, style);
        assertEquals(1, rootNote.getChildren().size());
        child = rootNote.getChildren().get(0);
        Assert.assertEquals("000001", child.getId());
        assertEquals(3, child.getChildren().size());
        grandChild = child.getChildren().get(0);
        assertEquals("000001", grandChild.getId());
        Assert.assertEquals(3, grandChild.getChildren().size());
        grandChild = child.getChildren().get(1);
        assertEquals("000002", grandChild.getId());
        Assert.assertEquals(0, grandChild.getChildren().size());
    }

    @Test
    public void testUpdateSourceOrWeight() throws Exception {
        ViewStyle style = ViewStyle.Basic.Forward.getStyle();
        Atom root = createAtom("wXu5g4v", "root");
        Note rootNote;
        String s;

        s = "" +
                "* :N5KBOAq: one\n";
        rootNote = wikiParser.parse(s);
        rootNote.setId(root.getId());
        queries.update(rootNote, 2, filter, style);
        Atom one = topicGraph.getAtomById("N5KBOAq").get();
        assertEquals(0.5f, one.getWeight());
        assertEquals(DefaultSources.PRIVATE, one.getSource());

        s = "" +
                "* :N5KBOAq: one\n" +
                "    @weight 0.75\n" +
                "    @source private\n";
        rootNote = wikiParser.parse(s);
        rootNote.setId(root.getId());
        queries.update(rootNote, 2, filter, style);
        assertEquals(0.75f, one.getWeight());
        assertEquals(DefaultSources.PRIVATE, one.getSource());
    }

    @Test
    public void testUpdateAlias() throws Exception {
        ViewStyle style = ViewStyle.Basic.Forward.getStyle();
        Atom root = createAtom("wXu5g4v", "root");
        Note rootNote;
        String s;

        s = "" +
                "* :N5KBOAq: one\n" +
                "    @alias http://example.org/ns/one\n";
        rootNote = wikiParser.parse(s);
        rootNote.setId(root.getId());
        queries.update(rootNote, 2, filter, style);
        Atom one = topicGraph.getAtomById("N5KBOAq").get();
        assertEquals("http://example.org/ns/one", one.getAlias());

        s = "" +
                "* :N5KBOAq: one\n" +
                "    @alias \n";
        rootNote = wikiParser.parse(s);
        rootNote.setId(root.getId());
        queries.update(rootNote, 2, filter, style);
        assertNull(one.getAlias());
    }

    @Test
    public void testUpdatePriority() throws Exception {
        ViewStyle style = ViewStyle.Basic.Forward.getStyle();
        Atom root = createAtom("0000000", "root");
        Note rootNote;
        String s;
        Atom one;

        s = "" +
                "* :0000001: one\n" +
                "    @priority 0.5";
        rootNote = wikiParser.parse(s);
        rootNote.setId(root.getId());
        queries.update(rootNote, 2, filter, style);
        one = topicGraph.getAtomById("0000001").get();
        assertEquals(0.5f, one.getPriority());

        // setting priority to 0 has the effect of removing the priority property from the note
        s = "" +
                "* :0000001: one\n" +
                "    @priority 0";
        rootNote = wikiParser.parse(s);
        rootNote.setId(root.getId());
        queries.update(rootNote, 2, filter, style);
        one = topicGraph.getAtomById("0000001").get();
        // TODO: allow removal of this property altogether
        assertEquals(0.0f, one.getPriority());
    }

    @Test
    public void childAndParentCountsAreCorrect() throws Exception {
        Note before = importNoteFromFile("io/wiki/wiki-example-3.txt");
        Atom root = createAtom("0000000", "root");
        root.setSource(DefaultSources.UNIVERSAL);
        before.setId(root.getId());
        queries.update(before, 2, filter, viewStyle);
        Note after = queries.view(root, 2, filter, viewStyle);

        assertEquals("root", after.getTitle());
        assertEquals(0, after.getNumberOfParents());
        assertEquals(3, after.getNumberOfChildren());

        Note child = after.getChildren().get(0);
        assertEquals("this is a public note", child.getTitle());
        assertEquals(1, child.getNumberOfParents());
        assertEquals(2, child.getNumberOfChildren());
    }

    @Test
    public void sourceIsCorrect() throws Exception {


    }

    @Test
    public void invisibleAtomsAreExcludedFromViews() throws Exception {
        Filter readFilter = new Filter(0f, 0.5f, DefaultSources.PUBLIC, DefaultSources.PUBLIC);
        Filter writeFilter = Filter.noFilter();
        ViewStyle style = ViewStyle.Basic.Forward.getStyle();

        Note rootNote = importNoteFromFile("io/wiki/wiki-example-3.txt");
        Atom root = createAtom("0000000", "root");
        root.setSource(DefaultSources.UNIVERSAL);
        rootNote.setId(root.getId());
        queries.update(rootNote, 2, writeFilter, style);

        Optional<Atom> a1 = topicGraph.getAtomById("0000001");
        assertEquals(DefaultSources.UNIVERSAL, a1.get().getSource());
        Optional<Atom> a2 = topicGraph.getAtomById("0000002");
        assertEquals(DefaultSources.PERSONAL, a2.get().getSource());

        Note after = queries.view(root, 2, readFilter, style);
        // the second atom in the list simply does not appear
        Assert.assertEquals(2, after.getNumberOfChildren());
        Assert.assertEquals(0, after.getNumberOfParents());
        List<Note> children = after.getChildren();
        Note n1 = children.get(0);
        Assert.assertEquals(1, n1.getNumberOfChildren());
        Assert.assertEquals(1, n1.getNumberOfParents());

        Note n5 = children.get(1);
        // children are not visible
        Assert.assertEquals(0, n5.getNumberOfChildren());
        Assert.assertEquals(1, n5.getNumberOfParents());

        List<Note> grandChildren = n1.getChildren();
        assertEquals(1, grandChildren.size());
        Note n3 = grandChildren.get(0);
        Assert.assertEquals(0, n3.getNumberOfChildren());
        Assert.assertEquals(1, n3.getNumberOfParents());
        assertEquals("this is a public child of a public note", n3.getTitle());
    }

    @Test
    public void invisibleAtomsAreSkippedDuringWrites() throws Exception {
        Filter readFilter = Filter.noFilter();
        Filter writeFilter = new Filter(0f, 0.5f, DefaultSources.PUBLIC, DefaultSources.PUBLIC);
        ViewStyle style = ViewStyle.Basic.Forward.getStyle();

        // three items in the private view
        Note rootNote = importNoteFromFile("io/wiki/wiki-example-3.txt");
        Atom root = createAtom("0000000", "note 0");
        root.setSource(DefaultSources.UNIVERSAL);
        rootNote.setId(root.getId());
        queries.update(rootNote, 2, Filter.noFilter(), style);
        rootNote = queries.view(root, 2, readFilter, style);
        assertEquals(3, rootNote.getNumberOfChildren());
        assertEquals(3, rootNote.getChildren().size());
        assertEquals(DefaultSources.PERSONAL, rootNote.getChildren().get(1).getSource());

        // add an atom in the public view
        rootNote = queries.view(root, 2, writeFilter, style);
        Note toAdd = new Note();
        toAdd.setId("0000007");
        toAdd.setTitle("note 7");
        toAdd.setSource(DefaultSources.UNIVERSAL);
        rootNote.getChildren().add(0, toAdd);
        queries.update(rootNote, 2, writeFilter, style);

        // four total items in the private view
        rootNote = queries.view(root, 2, readFilter, style);
        assertEquals(4, rootNote.getChildren().size());
        assertEquals("note 7", rootNote.getChildren().get(0).getTitle());
        rootNote = queries.view(root, 2, writeFilter, style);
        assertEquals(3, rootNote.getChildren().size());
        assertEquals("note 7", rootNote.getChildren().get(0).getTitle());
        rootNote.getChildren().remove(2);
        queries.update(rootNote, 2, writeFilter, style);

        rootNote = queries.view(root, 2, readFilter, style);
        assertEquals(3, rootNote.getChildren().size());
        assertEquals("note 7", rootNote.getChildren().get(0).getTitle());
        assertEquals("this is a public note", rootNote.getChildren().get(1).getTitle());
        assertEquals("this is a protected note", rootNote.getChildren().get(2).getTitle());
        rootNote = queries.view(root, 2, writeFilter, style);
        assertEquals(2, rootNote.getChildren().size());
        assertEquals("note 7", rootNote.getChildren().get(0).getTitle());
        assertEquals("this is a public note", rootNote.getChildren().get(1).getTitle());

        toAdd = new Note();
        toAdd.setId("0000008");
        toAdd.setTitle("note 8");
        toAdd.setSource(DefaultSources.UNIVERSAL);
        rootNote.getChildren().add(2, toAdd);
        queries.update(rootNote, 2, writeFilter, style);

        rootNote = queries.view(root, 2, readFilter, style);
        assertEquals(4, rootNote.getChildren().size());
        assertEquals("note 7", rootNote.getChildren().get(0).getTitle());
        assertEquals("this is a public note", rootNote.getChildren().get(1).getTitle());
        assertEquals("this is a protected note", rootNote.getChildren().get(2).getTitle());
        assertEquals("note 8", rootNote.getChildren().get(3).getTitle());
        rootNote = queries.view(root, 2, writeFilter, style);
        assertEquals(3, rootNote.getChildren().size());
        assertEquals("note 7", rootNote.getChildren().get(0).getTitle());
        assertEquals("this is a public note", rootNote.getChildren().get(1).getTitle());
        assertEquals("note 8", rootNote.getChildren().get(2).getTitle());
    }

    @Test
    public void testDontOverwriteNotesWithEmptyValues() throws Exception {
        ViewStyle style = ViewStyle.Basic.Forward.getStyle();

        String before = "* :000001: one\n" +
                "* :000002: two\n" +
                "* :000003: three";
        String after = "* :000001: ONE\n" +
                "* :000002:\n" +
                "* :000003: THREE";

        Note b = wikiParser.parse(before);
        Note a = wikiParser.parse(after);

        // First, check that 'after' was parsed correctly
        assertEquals(3, a.getChildren().size());
        assertEquals("000002", a.getChildren().get(1).getId());
        assertNull(a.getChildren().get(1).getTitle());

        Atom root = createAtom("000000", "root");

        b.setId(root.getId());
        queries.update(b, 2, filter, style);

        Atom a1 = topicGraph.getAtomById("000001").get();
        Atom a2 = topicGraph.getAtomById("000002").get();
        Atom a3 = topicGraph.getAtomById("000003").get();

        assertEquals("one", a1.getTitle());
        assertEquals("two", a2.getTitle());
        assertEquals("three", a3.getTitle());

        a.setId(root.getId());
        queries.update(a, 2, filter, style);

        // 000002's value was unaffected by the update
        assertEquals("ONE", a1.getTitle());
        assertEquals("two", a2.getTitle());
        assertEquals("THREE", a3.getTitle());
    }

    @Test
    public void testAddOnlyUpdate() throws Exception {
        ViewStyle style = ViewStyle.Basic.ForwardAddOnly.getStyle();

        String before = "* :000001: one\n" +
                "* :000002: two\n" +
                "* :000003: three";
        String after = "* :000004: four\n" +
                "* :000002: two";

        Note b = wikiParser.parse(before);
        Note a = wikiParser.parse(after);

        Atom root = createAtom("000000", "root");

        b.setId(root.getId());
        queries.update(b, 2, filter, style);

        Atom a1 = topicGraph.getAtomById("000001").get();
        Atom a2 = topicGraph.getAtomById("000002").get();
        Atom a3 = topicGraph.getAtomById("000003").get();

        assertEquals("one", a1.getTitle());
        assertEquals("two", a2.getTitle());
        assertEquals("three", a3.getTitle());
        assertEquals(3, EntityList.toJavaList(root.getChildren()).size());

        a.setId(root.getId());
        queries.update(a, 2, filter, style);

        Atom a4 = topicGraph.getAtomById("000004").get();

        assertEquals("four", a4.getTitle());
        List<Atom> children = EntityList.toJavaList(root.getChildren());
        assertEquals(4, children.size());
        assertEquals("four", children.get(0).getTitle());
        assertEquals("one", children.get(1).getTitle());
        assertEquals("two", children.get(2).getTitle());
    }

    @Test
    public void testFindRootsAndIsolatedAtoms() throws Exception {
        assertEquals(0, queries.findRootAtoms(filter, ViewStyle.Basic.Forward.getStyle(), 1).getChildren().size());
        assertEquals(0, queries.findRootAtoms(filter, ViewStyle.Basic.Backward.getStyle(), 1).getChildren().size());
        assertEquals(0, queries.findIsolatedAtoms(filter).getChildren().size());

        Atom atom0 = createAtom("000000", "0");

        assertEquals(1, queries.findRootAtoms(filter, ViewStyle.Basic.Forward.getStyle(), 1).getChildren().size());
        assertEquals(1, queries.findRootAtoms(filter, ViewStyle.Basic.Backward.getStyle(), 1).getChildren().size());
        assertEquals(1, queries.findIsolatedAtoms(filter).getChildren().size());

        Atom atom1 = createAtom("000001", "1");

        assertEquals(2, queries.findRootAtoms(filter, ViewStyle.Basic.Forward.getStyle(), 1).getChildren().size());
        assertEquals(2, queries.findRootAtoms(filter, ViewStyle.Basic.Backward.getStyle(), 1).getChildren().size());
        assertEquals(2, queries.findIsolatedAtoms(filter).getChildren().size());

        atom0.addChildAt(atom1, 0);

        assertEquals(1, queries.findRootAtoms(filter, ViewStyle.Basic.Forward.getStyle(), 1).getChildren().size());
        assertEquals(1, queries.findRootAtoms(filter, ViewStyle.Basic.Backward.getStyle(), 1).getChildren().size());
        assertEquals(0, queries.findIsolatedAtoms(filter).getChildren().size());

        Atom atom2 = createAtom("000002", "2");

        assertEquals(2, queries.findRootAtoms(filter, ViewStyle.Basic.Forward.getStyle(), 1).getChildren().size());
        assertEquals(2, queries.findRootAtoms(filter, ViewStyle.Basic.Backward.getStyle(), 1).getChildren().size());
        assertEquals(1, queries.findIsolatedAtoms(filter).getChildren().size());

        atom0.addChildAt(atom2, 0);

        assertEquals(1, queries.findRootAtoms(filter, ViewStyle.Basic.Forward.getStyle(), 1).getChildren().size());
        assertEquals(2, queries.findRootAtoms(filter, ViewStyle.Basic.Backward.getStyle(), 1).getChildren().size());
        assertEquals(0, queries.findIsolatedAtoms(filter).getChildren().size());
    }

    private void assertNotesEqual(final Atom a,
                                  final String... expected) {
        String[] actual = new String[countNotes(a)];

        int i = 0;
        EntityList<Atom> cur = a.getChildren();
        while (null != cur) {
            actual[i++] = cur.getFirst().getTitle();
            cur = cur.getRest();
        }

        assertArrayEquals(expected, actual);
    }

    private int countNotes(final Atom a) {
        EntityList cur = a.getChildren();
        int count = 0;
        while (cur != null) {
            count++;
            cur = cur.getRest();
        }

        return count;
    }
}
