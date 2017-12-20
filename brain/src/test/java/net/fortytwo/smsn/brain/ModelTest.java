package net.fortytwo.smsn.brain;

import net.fortytwo.smsn.brain.io.json.JsonPrinter;
import net.fortytwo.smsn.brain.model.Filter;
import net.fortytwo.smsn.brain.model.TopicGraph;
import net.fortytwo.smsn.brain.model.dto.NoteDTO;
import net.fortytwo.smsn.brain.model.entities.ListNode;
import net.fortytwo.smsn.brain.model.entities.Note;
import net.fortytwo.smsn.brain.query.Model;
import net.fortytwo.smsn.brain.query.ViewStyle;
import org.json.JSONObject;
import org.junit.Assert;
import org.junit.Test;

import java.io.IOException;
import java.util.List;

import static junit.framework.Assert.assertEquals;
import static org.junit.Assert.assertNull;

public class ModelTest extends BrainTestBase {
    private final JsonPrinter jsonPrinter = new JsonPrinter();

    @Override
    protected TopicGraph createTopicGraph() throws IOException {
        return createTinkerTopicGraph();
    }

    @Test
    public void simpleViewSucceeds() {
        ViewStyle style = ViewStyle.Basic.Forward.getStyle();

        Note root = createNote("00000", "The Earth");
        assertEquals("public", root.getSource());
        
    }

    @Test
    public void testEncoding() throws Exception {
        ViewStyle style = ViewStyle.Basic.Forward.getStyle();

        Note root = createNote("11111", "root");
        assertEquals("11111", root.getTopic().getId());

        Note rootNode = createNoteDTO();
        Model.setTopicId(rootNode, root.getTopic().getId());
        rootNode.setLabel("foo");
        Note child = createNoteDTO();
        child.setLabel("cheval \u00e0 phynances");
        rootNode.addChild(0, child);
        assertNull(child.getWeight());
        assertNull(child.getSource());
        assertNull(child.getCreated());

        model.view().root(root).height(1).filter(filter).style(style).put(rootNode);

        Note after = model.view().root(root).height(1).filter(filter).style(style).get();

        assertEquals("11111", Model.getTopicId(after));
        assertEquals("foo", after.getLabel());
        assertChildCount(1, after);

        JSONObject json = jsonPrinter.toJson(after);
        JSONObject j = json.getJSONArray("children").getJSONObject(0);
        assertEquals("cheval \u00e0 phynances", j.getString("title"));
    }

    @Test
    public void updateWithPageAndChildrenIsAccepted() throws Exception {
        Note root = createNote("11111", "root");

        Note tree = createNoteDTO();
        Model.setTopicId(tree, root.getTopic().getId());
        tree.setLabel("Arthur Dent");
        tree.setText("He's a jerk.\nA complete kneebiter.");
        Note child = createNoteDTO();
        child.setLabel("Random");
        Note.setChildren(child);

        model.view()
                .root(root).height(Integer.MAX_VALUE).filter(Filter.noFilter())
                .style(ViewStyle.Basic.Forward.getStyle())
                .put(tree);

        Note ad = getNote(root.getTopic());
        assertEquals(Filter.noFilter().getDefaultSource(), ad.getSource());
        assertEquals("Arthur Dent", ad.getLabel());
        assertEquals("He's a jerk.\nA complete kneebiter.", ad.getText());
        assertEquals(1, ListNode.toJavaList(ad.getFirst()).size());
        Note random = ad.getFirst().getFirst();
        assertEquals("Random", random.getLabel());
    }

    @Test
    public void testUpdateRecursion() throws Exception {
        ViewStyle style = ViewStyle.Basic.Forward.getStyle();
        Note root = createNote("wXu5g4v", "root");
        Note rootNode;
        String s;

        s = "" +
                "* :N5KBOAq: one\n" +
                "* :v8EuMtl: two\n" +
                "* :tOpwKho: three\n";
        rootNode = parseToNote(s);
        Model.setTopicId(rootNode, root.getTopic().getId());
        model.view().root(root).height(2).filter(filter).style(style).put(rootNode);
        assertNodesEqual(root, "one", "two", "three");

        Note one = getNote("N5KBOAq");
        Note two = getNote("v8EuMtl");

        s = "" +
                "* :N5KBOAq: one\n" +
                "    * :r4zU45R: ten\n" +
                "    * yellow\n" +
                "* :tOpwKho: three\n";
        rootNode = parseToNote(s);
        Model.setTopicId(rootNode, root.getTopic().getId());
        model.view().root(root).height(2).filter(filter).style(style).put(rootNode);
        // "two" has been removed
        assertNodesEqual(root, "one", "three");
        // grandchildren have been added
        assertNodesEqual(one, "ten", "yellow");
        Note ten = getNote("r4zU45R");

        s = "" +
                "* :N5KBOAq: one\n" +
                "    * :r4zU45R: ten\n" +
                "        * rabbit\n" +
                "    * purple\n" +
                "* :tOpwKho: three\n";
        rootNode = parseToNote(s);
        Model.setTopicId(rootNode, root.getTopic().getId());
        model.view().root(root).height(2).filter(filter).style(style).put(rootNode);
        // depth is only two, so "rabbit" is not reachable
        assertNodesEqual(ten);

        s = "" +
                "* :N5KBOAq: one\n" +
                "    * :r4zU45R: ten\n" +
                "    * green\n" +
                "        * rabbit\n" +
                "        * kangaroo\n" +
                "* :tOpwKho: three\n";
        rootNode = parseToNote(s);
        Model.setTopicId(rootNode, root.getTopic().getId());
        model.view().root(root).height(2).filter(filter).style(style).put(rootNode);
        Note green = one.getFirst().getRest().getFirst();
        // "rabbit" and "kangaroo" are added beneath "green" even though they're
        // deeper than 2 steps in the tree, because "green" is a new node
        assertNodesEqual(green, "rabbit", "kangaroo");

        s = "" +
                "* :v8EuMtl: two\n" +
                "* :tOpwKho: three\n";
        rootNode = parseToNote(s);
        Model.setTopicId(rootNode, root.getTopic().getId());
        model.view().root(root).height(2).filter(filter).style(style).put(rootNode);
        // "one" has been removed...
        assertNodesEqual(root, "two", "three");
        // but "one" still exists and has its previous nodes
        assertNodesEqual(one, "ten", "green");

        s = "" +
                "* :tOpwKho: three\n" +
                "    * red\n" +
                "* :v8EuMtl: two\n";
        rootNode = parseToNote(s);
        Model.setTopicId(rootNode, root.getTopic().getId());
        model.view().root(root).height(2).filter(filter).style(style).put(rootNode);
        // we swapped the order of "two" and "three"...
        assertNodesEqual(root, "three", "two");
        Note three = getNote("tOpwKho");
        // ...therefore, the children of "three" can't be modified in this update operation
        // (so "red" has been ignored)
        assertNodesEqual(three);

        s = "" +
                "* :v8EuMtl: two\n" +
                "    * elephant\n" +
                "* :v8EuMtl: two\n" +
                "* :tOpwKho: three\n";
        rootNode = parseToNote(s);
        Model.setTopicId(rootNode, root.getTopic().getId());
        model.view().root(root).height(2).filter(filter).style(style).put(rootNode);
        // duplicates are possible...
        assertNodesEqual(root, "two", "two", "three");
        // ...but when a duplicate is added, children of any matching duplicate will be ignored
        assertNodesEqual(two);

        s = "" +
                "* :v8EuMtl: two\n" +
                "    * elephant\n" +
                "* :v8EuMtl: two\n" +
                "    * gorilla\n" +
                "* :tOpwKho: three\n";
        rootNode = parseToNote(s);
        Model.setTopicId(rootNode, root.getTopic().getId());
        model.view().root(root).height(2).filter(filter).style(style).put(rootNode);
        assertNodesEqual(root, "two", "two", "three");
        // when duplicates already exist, children of duplicates follow the last-occurring instance
        assertNodesEqual(two, "gorilla");
    }

    @Test
    public void testPathologicalUpdateWithCycles() throws Exception {
        ViewStyle style = ViewStyle.Basic.Forward.getStyle();
        Note root = createNote("0000000", "root");
        Note rootNode, child, grandChild;
        String s;

        // OK to create an note which is its own parent
        s = "" +
                "* :000001: one\n" +
                "    * :000001: one\n";
        rootNode = parseToNote(s);
        Model.setTopicId(rootNode, root.getTopic().getId());
        model.view().root(root).height(2).filter(filter).style(style).put(rootNode);
        Note one = getNote("000001");
        Assert.assertEquals(1, ListNode.toJavaList(one.getFirst()).size());
        for (int i = 0; i < 2; i++) {
            assertChildCount(1, rootNode);
            child = rootNode.getFirst().get(0);
            Assert.assertEquals("000001", Model.getTopicId(child));
            assertChildCount(1, child);
            grandChild = child.getFirst().get(0);
            assertEquals("000001", Model.getTopicId(grandChild));

            rootNode = model.view().root(root).height(2).filter(filter).style(style).get();
        }

        // setting properties at the higher level has no effect, as we are pre-ordered w.r.t. updating of properties
        s = "" +
                "* :000001: one - updated\n" +
                "    * :000001: one\n";
        rootNode = parseToNote(s);
        Model.setTopicId(rootNode, root.getTopic().getId());
        model.view().root(root).height(2).filter(filter).style(style).put(rootNode);
        one = getNote("000001");
        Assert.assertEquals(1, ListNode.toJavaList(one.getFirst()).size());
        rootNode = model.view().root(root).height(2).filter(filter).style(style).get();
        assertChildCount(1, rootNode);
        child = rootNode.getFirst().get(0);
        Assert.assertEquals("000001", Model.getTopicId(child));
        Assert.assertEquals("one", child.getLabel());
        assertChildCount(1, child);
        grandChild = child.getFirst().get(0);
        assertEquals("000001", Model.getTopicId(grandChild));

        // setting properties at the lower level (the last visited) does have an effect
        s = "" +
                "* :000001: one\n" +
                "    * :000001: one - updated\n";
        rootNode = parseToNote(s);
        Model.setTopicId(rootNode, root.getTopic().getId());
        model.view().root(root).height(2).filter(filter).style(style).put(rootNode);
        one = getNote("000001");
        Assert.assertEquals(1, ListNode.toJavaList(one.getFirst()).size());
        rootNode = model.view().root(root).height(2).filter(filter).style(style).get();
        assertChildCount(1, rootNode);
        child = rootNode.getFirst().get(0);
        Assert.assertEquals("000001", Model.getTopicId(child));
        Assert.assertEquals("one - updated", child.getLabel());
        assertChildCount(1, child);
        grandChild = child.getFirst().get(0);
        assertEquals("000001", Model.getTopicId(grandChild));

        // the preorder rule does not apply when the link from parent to child is not repeated in the view;
        // the children of a note are updated only once
        s = "" +
                "* :000001: one\n" +
                "    * :000001: one\n" +
                "    * :000002: two\n";
        rootNode = parseToNote(s);
        Model.setTopicId(rootNode, root.getTopic().getId());
        model.view().root(root).height(2).filter(filter).style(style).put(rootNode);
        one = getNote("000001");
        Assert.assertEquals(2, ListNode.toJavaList(one.getFirst()).size());
        for (int i = 0; i < 2; i++) {
            assertChildCount(1, rootNode);
            child = rootNode.getFirst().get(0);
            Assert.assertEquals("000001", Model.getTopicId(child));
            assertChildCount(2, child);
            grandChild = child.getFirst().get(0);
            assertEquals("000001", Model.getTopicId(grandChild));
            grandChild = child.getFirst().get(1);
            assertEquals("000002", Model.getTopicId(grandChild));

            rootNode = model.view().root(root).height(2).filter(filter).style(style).get();
        }

        // get the height-3 view
        rootNode = model.view().root(root).height(3).filter(filter).style(style).get();
        assertChildCount(1, rootNode);
        child = rootNode.getFirst().get(0);
        Assert.assertEquals("000001", Model.getTopicId(child));
        assertChildCount(2, child);
        grandChild = child.getFirst().get(0);
        assertEquals("000001", Model.getTopicId(grandChild));
        assertChildCount(2, grandChild);
        grandChild = child.getFirst().get(1);
        assertEquals("000002", Model.getTopicId(grandChild));
        assertChildCount(0, grandChild);

        // adding or removing at the higher level has no effect, as we are pre-ordered w.r.t. updating of children
        s = "" +
                "* :000001: one\n" +
                "    * :000001: one\n" +
                "        * :000001: one\n" +
                "        * :000002: two\n" +
                "    * :000002: two\n" +
                "    * :000003: three\n";
        rootNode = parseToNote(s);
        Model.setTopicId(rootNode, root.getTopic().getId());
        model.view().root(root).height(3).filter(filter).style(style).put(rootNode);
        rootNode = model.view().root(root).height(3).filter(filter).style(style).get();
        assertChildCount(1, rootNode);
        child = rootNode.getFirst().get(0);
        Assert.assertEquals("000001", Model.getTopicId(child));
        assertChildCount(2, child);
        grandChild = child.getFirst().get(0);
        assertEquals("000001", Model.getTopicId(grandChild));
        assertChildCount(2, grandChild);
        grandChild = child.getFirst().get(1);
        assertEquals("000002", Model.getTopicId(grandChild));
        assertChildCount(0, grandChild);

        // adding or removing children at the lower level (the last visited) does have an effect
        s = "" +
                "* :000001: one\n" +
                "    * :000001: one\n" +
                "        * :000001: one\n" +
                "        * :000002: two\n" +
                "        * :000003: three\n" +
                "    * :000002: two\n";
        rootNode = parseToNote(s);
        Model.setTopicId(rootNode, root.getTopic().getId());
        model.view().root(root).height(3).filter(filter).style(style).put(rootNode);
        rootNode = model.view().root(root).height(3).filter(filter).style(style).get();
        assertChildCount(1, rootNode);
        child = rootNode.getFirst().get(0);
        Assert.assertEquals("000001", Model.getTopicId(child));
        assertChildCount(3, child);
        grandChild = child.getFirst().get(0);
        assertEquals("000001", Model.getTopicId(grandChild));
        assertChildCount(3, grandChild);
        grandChild = child.getFirst().get(1);
        assertEquals("000002", Model.getTopicId(grandChild));
        assertChildCount(0, grandChild);
    }

    @Test
    public void testUpdateSourceOrWeight() throws Exception {
        ViewStyle style = ViewStyle.Basic.Forward.getStyle();
        Note root = createNote("wXu5g4v", "root");
        Note rootNode;
        String s;

        s = "" +
                "* :N5KBOAq: one\n";
        rootNode = parseToNote(s);
        Model.setTopicId(rootNode, root.getTopic().getId());
        model.view().root(root).height(2).filter(filter).style(style).put(rootNode);
        Note one = getNote("N5KBOAq");
        assertEquals(0.5f, one.getWeight());
        assertEquals(DefaultSources.PRIVATE, one.getSource());

        s = "" +
                "* :N5KBOAq: one\n" +
                "    @weight 0.75\n" +
                "    @source private\n";
        rootNode = parseToNote(s);
        Model.setTopicId(rootNode, root.getTopic().getId());
        model.view().root(root).height(2).filter(filter).style(style).put(rootNode);
        assertEquals(0.75f, one.getWeight());
        assertEquals(DefaultSources.PRIVATE, one.getSource());
    }

    @Test
    public void testUpdateAlias() throws Exception {
        ViewStyle style = ViewStyle.Basic.Forward.getStyle();
        Note root = createNote("wXu5g4v", "root");
        Note rootNode;
        String s;

        s = "" +
                "* :N5KBOAq: one\n" +
                "    @alias http://example.org/ns/one\n";
        rootNode = parseToNote(s);
        Model.setTopicId(rootNode, root.getTopic().getId());
        model.view().root(root).height(2).filter(filter).style(style).put(rootNode);
        Note one = getNote("N5KBOAq");
        assertEquals("http://example.org/ns/one", one.getAlias());

        s = "" +
                "* :N5KBOAq: one\n" +
                "    @alias \n";
        rootNode = parseToNote(s);
        Model.setTopicId(rootNode, root.getTopic().getId());
        model.view().root(root).height(2).filter(filter).style(style).put(rootNode);
        assertNull(one.getAlias());
    }

    @Test
    public void testUpdatePriority() throws Exception {
        ViewStyle style = ViewStyle.Basic.Forward.getStyle();
        Note root = createNote("0000000", "root");
        Note rootNode;
        String s;
        Note one;

        s = "" +
                "* :0000001: one\n" +
                "    @priority 0.5";
        rootNode = parseToNote(s);
        Model.setTopicId(rootNode, root.getTopic().getId());
        model.view().root(root).height(2).filter(filter).style(style).put(rootNode);
        one = getNote("0000001");
        assertEquals(0.5f, one.getPriority());

        // setting priority to 0 has the effect of removing the priority property from the node
        s = "" +
                "* :0000001: one\n" +
                "    @priority 0";
        rootNode = parseToNote(s);
        Model.setTopicId(rootNode, root.getTopic().getId());
        model.view().root(root).height(2).filter(filter).style(style).put(rootNode);
        one = getNote("0000001");
        // TODO: allow removal of this property altogether
        assertEquals(0.0f, one.getPriority());
    }

    @Test
    public void childAndParentCountsAreCorrect() throws Exception {
        Note node = importNodeFromFile("io/wiki/wiki-example-3.txt");
        Note root = createNote("0000000", "root");
        root.setSource(DefaultSources.UNIVERSAL);
        Model.setTopicId(node, root.getTopic().getId());
        assertEquals("root", root.getLabel());

        Note after = model.view()
                .root(root).height(2).filter(filter).style(viewStyle)
                .put(node).get();

        assertEquals("Arthur Dent", after.getLabel());
        assertEquals("Arthur Dent", root.getLabel());
        assertEquals(0, after.getNumberOfParents());
        assertEquals(3, after.getNumberOfChildren());

        Note child = after.getFirst().get(0);
        assertEquals("this is a public note", child.getLabel());
        assertEquals(1, child.getNumberOfParents());
        assertEquals(2, child.getNumberOfChildren());
    }

    @Test
    public void invisibleNotesAreExcludedFromViews() throws Exception {
        Filter readFilter = new Filter(0f, 0.5f, DefaultSources.PUBLIC, DefaultSources.PUBLIC);
        Filter writeFilter = Filter.noFilter();
        ViewStyle style = ViewStyle.Basic.Forward.getStyle();

        Note rootNode = importNodeFromFile("io/wiki/wiki-example-3.txt");
        Note root = createNote("0000000", "root");
        root.setSource(DefaultSources.UNIVERSAL);
        Model.setTopicId(rootNode, root.getTopic().getId());
        model.view().root(root).height(2).filter(writeFilter).style(style).put(rootNode);

        Note a1 = getNote("0000001");
        assertEquals(DefaultSources.UNIVERSAL, a1.getSource());
        Note a2 = getNote("0000002");
        assertEquals(DefaultSources.PERSONAL, a2.getSource());

        Note after = model.view().root(root).height(2).filter(readFilter).style(style).get();
        // the second note in the list simply does not appear
        Assert.assertEquals(2, after.getNumberOfChildren());
        Assert.assertEquals(0, after.getNumberOfParents());
        ListNode<Note> children = after.getFirst();
        Note n1 = children.get(0);
        Assert.assertEquals(1, n1.getNumberOfChildren());
        Assert.assertEquals(1, n1.getNumberOfParents());

        Note n5 = children.get(1);
        // children are not visible
        Assert.assertEquals(0, n5.getNumberOfChildren());
        Assert.assertEquals(1, n5.getNumberOfParents());

        ListNode<Note> grandChildren = n1.getFirst();
        assertEquals(1, grandChildren.length());
        Note n3 = grandChildren.get(0);
        Assert.assertEquals(0, n3.getNumberOfChildren());
        Assert.assertEquals(1, n3.getNumberOfParents());
        assertEquals("this is a public child of a public note", n3.getLabel());
    }

    @Test
    public void invisibleNotesAreSkippedDuringWrites() throws Exception {
        Filter readFilter = Filter.noFilter();
        Filter writeFilter = new Filter(0f, 0.5f, DefaultSources.PUBLIC, DefaultSources.PUBLIC);
        ViewStyle style = ViewStyle.Basic.Forward.getStyle();

        // three items in the private view
        Note rootNode = importNodeFromFile("io/wiki/wiki-example-3.txt");
        Note root = createNote("0000000", "node 0");
        root.setSource(DefaultSources.UNIVERSAL);
        Model.setTopicId(rootNode, root.getTopic().getId());
        model.view().root(root).height(2).filter(Filter.noFilter()).style(style).put(rootNode);
        rootNode = model.view().root(root).height(2).filter(readFilter).style(style).get();
        assertEquals(3, rootNode.getNumberOfChildren());
        assertChildCount(3, rootNode);
        assertEquals(DefaultSources.PERSONAL, rootNode.getFirst().get(1).getSource());

        // add an note in the public view
        rootNode = model.view().root(root).height(2).filter(writeFilter).style(style).get();
        Note toAdd = createNoteDTO();
        Model.setTopicId(toAdd, "0000007");
        toAdd.setLabel("node 7");
        toAdd.setSource(DefaultSources.UNIVERSAL);
        rootNode.getFirst().addChild(0, toAdd);
        model.view().root(root).height(2).filter(writeFilter).style(style).put(rootNode);

        // four total items in the private view
        rootNode = model.view().root(root).height(2).filter(readFilter).style(style).get();
        assertChildCount(4, rootNode);
        assertEquals("node 7", rootNode.getFirst().get(0).getLabel());
        rootNode = model.view().root(root).height(2).filter(writeFilter).style(style).get();
        assertChildCount(3, rootNode);
        assertEquals("node 7", rootNode.getFirst().get(0).getLabel());
        rootNode.getFirst().remove(2);
        model.view().root(root).height(2).filter(writeFilter).style(style).put(rootNode);

        rootNode = model.view().root(root).height(2).filter(readFilter).style(style).get();
        assertChildCount(3, rootNode);
        assertEquals("node 7", rootNode.getFirst().get(0).getLabel());
        assertEquals("this is a public note", rootNode.getFirst().get(1).getLabel());
        assertEquals("this is a protected note", rootNode.getFirst().get(2).getLabel());
        rootNode = model.view().root(root).height(2).filter(writeFilter).style(style).get();
        assertChildCount(2, rootNode);
        assertEquals("node 7", rootNode.getFirst().get(0).getLabel());
        assertEquals("this is a public note", rootNode.getFirst().get(1).getLabel());

        toAdd = createNoteDTO();
        Model.setTopicId(toAdd, "0000008");
        toAdd.setLabel("node 8");
        toAdd.setSource(DefaultSources.UNIVERSAL);
        rootNode.getFirst().add(2, toAdd);
        model.view().root(root).height(2).filter(writeFilter).style(style).put(rootNode);

        rootNode = model.view().root(root).height(2).filter(readFilter).style(style).get();
        assertChildCount(4, rootNode);
        assertEquals("node 7", rootNode.getFirst().get(0).getLabel());
        assertEquals("this is a public note", rootNode.getFirst().get(1).getLabel());
        assertEquals("this is a protected note", rootNode.getFirst().get(2).getLabel());
        assertEquals("node 8", rootNode.getFirst().get(3).getLabel());
        rootNode = model.view().root(root).height(2).filter(writeFilter).style(style).get();
        assertChildCount(3, rootNode);
        assertEquals("node 7", rootNode.getFirst().get(0).getLabel());
        assertEquals("this is a public note", rootNode.getFirst().get(1).getLabel());
        assertEquals("node 8", rootNode.getFirst().get(2).getLabel());
    }

    @Test
    public void testDontOverwriteNodesWithEmptyValues() throws Exception {
        ViewStyle style = ViewStyle.Basic.Forward.getStyle();

        String before = "* :000001: one\n" +
                "* :000002: two\n" +
                "* :000003: three";
        String after = "* :000001: ONE\n" +
                "* :000002:\n" +
                "* :000003: THREE";

        Note b = parseToNote(before);
        Note a = parseToNote(after);

        // First, check that 'after' was parsed correctly
        assertChildCount(3, a);
        assertEquals("000002", Model.getTopicId(a.getFirst().get(1)));
        assertNull(a.getFirst().get(1).getLabel());

        Note root = createNote("000000", "root");

        Model.setTopicId(b, root.getTopic().getId());
        model.view().root(root).height(2).filter(filter).style(style).put(b);

        Note a1 = getNote("000001");
        Note a2 = getNote("000002");
        Note a3 = getNote("000003");

        assertEquals("one", a1.getLabel());
        assertEquals("two", a2.getLabel());
        assertEquals("three", a3.getLabel());

        Model.setTopicId(a, root.getTopic().getId());
        model.view().root(root).height(2).filter(filter).style(style).put(a);

        // 000002's value was unaffected by the update
        assertEquals("ONE", a1.getLabel());
        assertEquals("two", a2.getLabel());
        assertEquals("THREE", a3.getLabel());
    }

    @Test
    public void testAddOnlyUpdate() throws Exception {
        ViewStyle style = ViewStyle.Basic.ForwardAddOnly.getStyle();

        String before = "* :000001: one\n" +
                "* :000002: two\n" +
                "* :000003: three";
        String after = "* :000004: four\n" +
                "* :000002: two";

        Note b = parseToNote(before);
        Note a = parseToNote(after);

        Note root = createNote("000000", "root");

        Model.setTopicId(b, root.getTopic().getId());
        model.view().root(root).height(2).filter(filter).style(style).put(b);

        Note a1 = getNote("000001");
        Note a2 = getNote("000002");
        Note a3 = getNote("000003");

        assertEquals("one", a1.getLabel());
        assertEquals("two", a2.getLabel());
        assertEquals("three", a3.getLabel());
        assertEquals(3, ListNode.toJavaList(root.getFirst()).size());

        Model.setTopicId(a, root.getTopic().getId());
        model.view().root(root).height(2).filter(filter).style(style).put(a);

        Note a4 = getNote("000004");

        assertEquals("four", a4.getLabel());
        List<Note> children = ListNode.toJavaList(root.getFirst());
        assertEquals(4, children.size());
        assertEquals("four", children.get(0).getLabel());
        assertEquals("one", children.get(1).getLabel());
        assertEquals("two", children.get(2).getLabel());
    }

    /*
    @Test
    public void testFindRootsAndIsolatedNotes() throws Exception {
        assertChildCount(0, queries.findRootNotes(filter, ViewStyle.Basic.Forward.getStyle(), 1));
        assertChildCount(0, queries.findRootNotes(filter, ViewStyle.Basic.Backward.getStyle(), 1));
        //assertChildCount(0, queries.findIsolatedNotes(filter));

        Note note0 = createNote("000000", "0");

        assertChildCount(1, queries.findRootNotes(filter, ViewStyle.Basic.Forward.getStyle(), 1));
        assertChildCount(1, queries.findRootNotes(filter, ViewStyle.Basic.Backward.getStyle(), 1));
        //assertChildCount(1, queries.findIsolatedNotes(filter));

        Note note1 = createNote("000001", "1");

        assertChildCount(2, queries.findRootNotes(filter, ViewStyle.Basic.Forward.getStyle(), 1));
        assertChildCount(2, queries.findRootNotes(filter, ViewStyle.Basic.Backward.getStyle(), 1));
        //assertChildCount(2, queries.findIsolatedNotes(filter));

        note0.addChild(0, note1);

        assertChildCount(1, queries.findRootNotes(filter, ViewStyle.Basic.Forward.getStyle(), 1));
        assertChildCount(1, queries.findRootNotes(filter, ViewStyle.Basic.Backward.getStyle(), 1));
        //assertChildCount(0, queries.findIsolatedNotes(filter));

        Note note2 = createNote("000002", "2");

        assertChildCount(2, queries.findRootNotes(filter, ViewStyle.Basic.Forward.getStyle(), 1));
        assertChildCount(2, queries.findRootNotes(filter, ViewStyle.Basic.Backward.getStyle(), 1));
        //assertChildCount(1, queries.findIsolatedNotes(filter));

        note0.addChild(0, note2);

        assertChildCount(1, queries.findRootNotes(filter, ViewStyle.Basic.Forward.getStyle(), 1));
        assertChildCount(2, queries.findRootNotes(filter, ViewStyle.Basic.Backward.getStyle(), 1));
        //assertChildCount(0, queries.findIsolatedNotes(filter));
    }
    */

    private Note createNoteDTO() {
        return new NoteDTO();
    }
}
