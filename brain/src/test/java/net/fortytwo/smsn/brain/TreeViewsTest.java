package net.fortytwo.smsn.brain;

import net.fortytwo.smsn.brain.io.json.JsonPrinter;
import net.fortytwo.smsn.brain.model.Filter;
import net.fortytwo.smsn.brain.model.TopicGraph;
import net.fortytwo.smsn.brain.model.dto.LinkDTO;
import net.fortytwo.smsn.brain.model.dto.PageDTO;
import net.fortytwo.smsn.brain.model.dto.TreeNodeDTO;
import net.fortytwo.smsn.brain.model.entities.Note;
import net.fortytwo.smsn.brain.model.entities.Link;
import net.fortytwo.smsn.brain.model.entities.ListNode;
import net.fortytwo.smsn.brain.model.entities.TreeNode;
import net.fortytwo.smsn.brain.query.TreeViews;
import net.fortytwo.smsn.brain.query.ViewStyle;
import org.json.JSONObject;
import org.junit.Assert;
import org.junit.Test;

import java.io.IOException;
import java.util.List;
import java.util.Optional;

import static junit.framework.Assert.assertEquals;
import static org.junit.Assert.assertNull;

public class TreeViewsTest extends BrainTestBase {
    private final JsonPrinter jsonPrinter = new JsonPrinter();

    @Override
    protected TopicGraph createTopicGraph() throws IOException {
        return createTinkerTopicGraph();
    }
    
    @Test
    public void testEncoding() throws Exception {
        ViewStyle style = ViewStyle.Basic.Forward.getStyle();

        Note root = createAtom("11111", "root");
        assertEquals("11111", root.getId());

        TreeNode<Link> rootNode = createTree();
        TreeViews.setId(rootNode, root.getId());
        TreeViews.setTitle(rootNode, "foo");
        TreeNode<Link> child = createTree();
        TreeViews.setTitle(child, "cheval \u00e0 phynances");
        rootNode.addChild(child);
        assertNull(TreeViews.getWeight(child));
        assertNull(TreeViews.getSource(child));
        assertNull(TreeViews.getCreated(child));

        queries.update(rootNode, 1, filter, style);

        TreeNode<Link> after = queries.view(root, 1, filter, style);

        assertEquals("11111", TreeViews.getId(after));
        assertEquals("foo", TreeViews.getTitle(after));
        assertChildCount(1, after);

        JSONObject json = jsonPrinter.toJson(after);
        JSONObject j = json.getJSONArray("children").getJSONObject(0);
        assertEquals("cheval \u00e0 phynances", j.getString("title"));
    }

    @Test
    public void updateWithPageAndChildrenIsAccepted() throws Exception {
        Note root = createAtom("11111", "root");

        TreeNode<Link> tree = createTree();
        TreeViews.setId(tree, root.getId());
        TreeViews.setTitle(tree, "Arthur Dent");
        TreeViews.setText(tree, "He's a jerk.\nA complete kneebiter.");
        TreeNode<Link> child = createTree();
        TreeViews.setTitle(child, "Random");
        tree.addChild(child);

        queries.update(tree, Integer.MAX_VALUE, Filter.noFilter(), ViewStyle.Basic.Forward.getStyle());

        Note ad = topicGraph.getNotesById(root.getId()).get();
        assertEquals(Filter.noFilter().getDefaultSource(), ad.getSource());
        assertEquals("Arthur Dent", ad.getTitle());
        assertEquals("He's a jerk.\nA complete kneebiter.", ad.getText());
        assertEquals(1, ListNode.toJavaList(ad.getChildren()).size());
        Note random = ad.getChildren().getFirst();
        assertEquals("Random", random.getTitle());
    }

    @Test
    public void testUpdateRecursion() throws Exception {
        ViewStyle style = ViewStyle.Basic.Forward.getStyle();
        Note root = createAtom("wXu5g4v", "root");
        TreeNode<Link> rootNode;
        String s;

        s = "" +
                "* :N5KBOAq: one\n" +
                "* :v8EuMtl: two\n" +
                "* :tOpwKho: three\n";
        rootNode = parseToTree(s);
        TreeViews.setId(rootNode, root.getId());
        queries.update(rootNode, 2, filter, style);
        assertNodesEqual(root, "one", "two", "three");

        Note one = topicGraph.getNotesById("N5KBOAq").get();
        Note two = topicGraph.getNotesById("v8EuMtl").get();

        s = "" +
                "* :N5KBOAq: one\n" +
                "    * :r4zU45R: ten\n" +
                "    * yellow\n" +
                "* :tOpwKho: three\n";
        rootNode = parseToTree(s);
        TreeViews.setId(rootNode, root.getId());
        queries.update(rootNode, 2, filter, style);
        // "two" has been removed
        assertNodesEqual(root, "one", "three");
        // grandchildren have been added
        assertNodesEqual(one, "ten", "yellow");
        Note ten = topicGraph.getNotesById("r4zU45R").get();

        s = "" +
                "* :N5KBOAq: one\n" +
                "    * :r4zU45R: ten\n" +
                "        * rabbit\n" +
                "    * purple\n" +
                "* :tOpwKho: three\n";
        rootNode = parseToTree(s);
        TreeViews.setId(rootNode, root.getId());
        queries.update(rootNode, 2, filter, style);
        // depth is only two, so "rabbit" is not reachable
        assertNodesEqual(ten);

        s = "" +
                "* :N5KBOAq: one\n" +
                "    * :r4zU45R: ten\n" +
                "    * green\n" +
                "        * rabbit\n" +
                "        * kangaroo\n" +
                "* :tOpwKho: three\n";
        rootNode = parseToTree(s);
        TreeViews.setId(rootNode, root.getId());
        queries.update(rootNode, 2, filter, style);
        Note green = one.getChildren().getRest().getFirst();
        // "rabbit" and "kangaroo" are added beneath "green" even though they're
        // deeper than 2 steps in the tree, because "green" is a new node
        assertNodesEqual(green, "rabbit", "kangaroo");

        s = "" +
                "* :v8EuMtl: two\n" +
                "* :tOpwKho: three\n";
        rootNode = parseToTree(s);
        TreeViews.setId(rootNode, root.getId());
        queries.update(rootNode, 2, filter, style);
        // "one" has been removed...
        assertNodesEqual(root, "two", "three");
        // but "one" still exists and has its previous nodes
        assertNodesEqual(one, "ten", "green");

        s = "" +
                "* :tOpwKho: three\n" +
                "    * red\n" +
                "* :v8EuMtl: two\n";
        rootNode = parseToTree(s);
        TreeViews.setId(rootNode, root.getId());
        queries.update(rootNode, 2, filter, style);
        // we swapped the order of "two" and "three"...
        assertNodesEqual(root, "three", "two");
        Note three = topicGraph.getNotesById("tOpwKho").get();
        // ...therefore, the children of "three" can't be modified in this update operation
        // (so "red" has been ignored)
        assertNodesEqual(three);

        s = "" +
                "* :v8EuMtl: two\n" +
                "    * elephant\n" +
                "* :v8EuMtl: two\n" +
                "* :tOpwKho: three\n";
        rootNode = parseToTree(s);
        TreeViews.setId(rootNode, root.getId());
        queries.update(rootNode, 2, filter, style);
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
        rootNode = parseToTree(s);
        TreeViews.setId(rootNode, root.getId());
        queries.update(rootNode, 2, filter, style);
        assertNodesEqual(root, "two", "two", "three");
        // when duplicates already exist, children of duplicates follow the last-occurring instance
        assertNodesEqual(two, "gorilla");
    }

    @Test
    public void testPathologicalUpdateWithCycles() throws Exception {
        ViewStyle style = ViewStyle.Basic.Forward.getStyle();
        Note root = createAtom("0000000", "root");
        TreeNode<Link> rootNode, child, grandChild;
        String s;

        // OK to create an atom which is its own parent
        s = "" +
                "* :000001: one\n" +
                "    * :000001: one\n";
        rootNode = parseToTree(s);
        TreeViews.setId(rootNode, root.getId());
        queries.update(rootNode, 2, filter, style);
        Note one = topicGraph.getNotesById("000001").get();
        Assert.assertEquals(1, ListNode.toJavaList(one.getChildren()).size());
        for (int i = 0; i < 2; i++) {
            assertChildCount(1, rootNode);
            child = rootNode.getChildren().get(0);
            Assert.assertEquals("000001", TreeViews.getId(child));
            assertChildCount(1, child);
            grandChild = child.getChildren().get(0);
            assertEquals("000001", TreeViews.getId(grandChild));

            rootNode = queries.view(root, 2, filter, style);
        }

        // setting properties at the higher level has no effect, as we are pre-ordered w.r.t. updating of properties
        s = "" +
                "* :000001: one - updated\n" +
                "    * :000001: one\n";
        rootNode = parseToTree(s);
        TreeViews.setId(rootNode, root.getId());
        queries.update(rootNode, 2, filter, style);
        one = topicGraph.getNotesById("000001").get();
        Assert.assertEquals(1, ListNode.toJavaList(one.getChildren()).size());
        rootNode = queries.view(root, 2, filter, style);
        assertChildCount(1, rootNode);
        child = rootNode.getChildren().get(0);
        Assert.assertEquals("000001", TreeViews.getId(child));
        Assert.assertEquals("one", TreeViews.getTitle(child));
        assertChildCount(1, child);
        grandChild = child.getChildren().get(0);
        assertEquals("000001", TreeViews.getId(grandChild));

        // setting properties at the lower level (the last visited) does have an effect
        s = "" +
                "* :000001: one\n" +
                "    * :000001: one - updated\n";
        rootNode = parseToTree(s);
        TreeViews.setId(rootNode, root.getId());
        queries.update(rootNode, 2, filter, style);
        one = topicGraph.getNotesById("000001").get();
        Assert.assertEquals(1, ListNode.toJavaList(one.getChildren()).size());
        rootNode = queries.view(root, 2, filter, style);
        assertChildCount(1, rootNode);
        child = rootNode.getChildren().get(0);
        Assert.assertEquals("000001", TreeViews.getId(child) );
        Assert.assertEquals("one - updated", TreeViews.getTitle(child));
        assertChildCount(1, child);
        grandChild = child.getChildren().get(0);
        assertEquals("000001", TreeViews.getId(grandChild) );

        // the preorder rule does not apply when the link from parent to child is not repeated in the view;
        // the children of an atom are updated only once
        s = "" +
                "* :000001: one\n" +
                "    * :000001: one\n" +
                "    * :000002: two\n";
        rootNode = parseToTree(s);
        TreeViews.setId(rootNode, root.getId());
        queries.update(rootNode, 2, filter, style);
        one = topicGraph.getNotesById("000001").get();
        Assert.assertEquals(2, ListNode.toJavaList(one.getChildren()).size());
        for (int i = 0; i < 2; i++) {
            assertChildCount(1, rootNode);
            child = rootNode.getChildren().get(0);
            Assert.assertEquals("000001", TreeViews.getId(child) );
            assertChildCount(2, child);
            grandChild = child.getChildren().get(0);
            assertEquals("000001", TreeViews.getId(grandChild) );
            grandChild = child.getChildren().get(1);
            assertEquals("000002", TreeViews.getId(grandChild) );

            rootNode = queries.view(root, 2, filter, style);
        }

        // get the height-3 view
        rootNode = queries.view(root, 3, filter, style);
        assertChildCount(1, rootNode);
        child = rootNode.getChildren().get(0);
        Assert.assertEquals("000001", TreeViews.getId(child) );
        assertChildCount(2, child);
        grandChild = child.getChildren().get(0);
        assertEquals("000001", TreeViews.getId(grandChild) );
        assertChildCount(2, grandChild);
        grandChild = child.getChildren().get(1);
        assertEquals("000002", TreeViews.getId(grandChild) );
        assertChildCount(0, grandChild);

        // adding or removing at the higher level has no effect, as we are pre-ordered w.r.t. updating of children
        s = "" +
                "* :000001: one\n" +
                "    * :000001: one\n" +
                "        * :000001: one\n" +
                "        * :000002: two\n" +
                "    * :000002: two\n" +
                "    * :000003: three\n";
        rootNode = parseToTree(s);
        TreeViews.setId(rootNode, root.getId());
        queries.update(rootNode, 3, filter, style);
        rootNode = queries.view(root, 3, filter, style);
        assertChildCount(1, rootNode);
        child = rootNode.getChildren().get(0);
        Assert.assertEquals("000001", TreeViews.getId(child) );
        assertChildCount(2, child);
        grandChild = child.getChildren().get(0);
        assertEquals("000001", TreeViews.getId(grandChild) );
        assertChildCount(2, grandChild);
        grandChild = child.getChildren().get(1);
        assertEquals("000002", TreeViews.getId(grandChild) );
        assertChildCount(0, grandChild);

        // adding or removing children at the lower level (the last visited) does have an effect
        s = "" +
                "* :000001: one\n" +
                "    * :000001: one\n" +
                "        * :000001: one\n" +
                "        * :000002: two\n" +
                "        * :000003: three\n" +
                "    * :000002: two\n";
        rootNode = parseToTree(s);
        TreeViews.setId(rootNode, root.getId());
        queries.update(rootNode, 3, filter, style);
        rootNode = queries.view(root, 3, filter, style);
        assertChildCount(1, rootNode);
        child = rootNode.getChildren().get(0);
        Assert.assertEquals("000001", TreeViews.getId(child) );
        assertChildCount(3, child);
        grandChild = child.getChildren().get(0);
        assertEquals("000001", TreeViews.getId(grandChild) );
        assertChildCount(3, grandChild);
        grandChild = child.getChildren().get(1);
        assertEquals("000002", TreeViews.getId(grandChild) );
        assertChildCount(0, grandChild);
    }

    @Test
    public void testUpdateSourceOrWeight() throws Exception {
        ViewStyle style = ViewStyle.Basic.Forward.getStyle();
        Note root = createAtom("wXu5g4v", "root");
        TreeNode<Link> rootNode;
        String s;

        s = "" +
                "* :N5KBOAq: one\n";
        rootNode = parseToTree(s);
        TreeViews.setId(rootNode, root.getId());
        queries.update(rootNode, 2, filter, style);
        Note one = topicGraph.getNotesById("N5KBOAq").get();
        assertEquals(0.5f, one.getWeight());
        assertEquals(DefaultSources.PRIVATE, one.getSource());

        s = "" +
                "* :N5KBOAq: one\n" +
                "    @weight 0.75\n" +
                "    @source private\n";
        rootNode = parseToTree(s);
        TreeViews.setId(rootNode, root.getId());
        queries.update(rootNode, 2, filter, style);
        assertEquals(0.75f, one.getWeight());
        assertEquals(DefaultSources.PRIVATE, one.getSource());
    }

    @Test
    public void testUpdateAlias() throws Exception {
        ViewStyle style = ViewStyle.Basic.Forward.getStyle();
        Note root = createAtom("wXu5g4v", "root");
        TreeNode<Link> rootNode;
        String s;

        s = "" +
                "* :N5KBOAq: one\n" +
                "    @alias http://example.org/ns/one\n";
        rootNode = parseToTree(s);
        TreeViews.setId(rootNode, root.getId());
        queries.update(rootNode, 2, filter, style);
        Note one = topicGraph.getNotesById("N5KBOAq").get();
        assertEquals("http://example.org/ns/one", one.getAlias());

        s = "" +
                "* :N5KBOAq: one\n" +
                "    @alias \n";
        rootNode = parseToTree(s);
        TreeViews.setId(rootNode, root.getId());
        queries.update(rootNode, 2, filter, style);
        assertNull(one.getAlias());
    }

    @Test
    public void testUpdatePriority() throws Exception {
        ViewStyle style = ViewStyle.Basic.Forward.getStyle();
        Note root = createAtom("0000000", "root");
        TreeNode<Link> rootNode;
        String s;
        Note one;

        s = "" +
                "* :0000001: one\n" +
                "    @priority 0.5";
        rootNode = parseToTree(s);
        TreeViews.setId(rootNode, root.getId());
        queries.update(rootNode, 2, filter, style);
        one = topicGraph.getNotesById("0000001").get();
        assertEquals(0.5f, one.getPriority());

        // setting priority to 0 has the effect of removing the priority property from the node
        s = "" +
                "* :0000001: one\n" +
                "    @priority 0";
        rootNode = parseToTree(s);
        TreeViews.setId(rootNode, root.getId());
        queries.update(rootNode, 2, filter, style);
        one = topicGraph.getNotesById("0000001").get();
        // TODO: allow removal of this property altogether
        assertEquals(0.0f, one.getPriority());
    }

    @Test
    public void childAndParentCountsAreCorrect() throws Exception {
        TreeNode<Link> node = importNodeFromFile("io/wiki/wiki-example-3.txt");
        Note root = createAtom("0000000", "root");
        root.setSource(DefaultSources.UNIVERSAL);
        TreeViews.setId(node, root.getId());
        assertEquals("root", root.getTitle());

        queries.update(node, 2, filter, viewStyle);
        TreeNode<Link> after = queries.view(root, 2, filter, viewStyle);

        assertEquals("Arthur Dent", TreeViews.getTitle(after));
        assertEquals("Arthur Dent", root.getTitle());
        assertEquals(0, after.getNumberOfParents());
        assertEquals(3, after.getNumberOfChildren());

        TreeNode<Link> child = after.getChildren().get(0);
        assertEquals("this is a public note", TreeViews.getTitle(child));
        assertEquals(1, child.getNumberOfParents());
        assertEquals(2, child.getNumberOfChildren());
    }

    @Test
    public void invisibleAtomsAreExcludedFromViews() throws Exception {
        Filter readFilter = new Filter(0f, 0.5f, DefaultSources.PUBLIC, DefaultSources.PUBLIC);
        Filter writeFilter = Filter.noFilter();
        ViewStyle style = ViewStyle.Basic.Forward.getStyle();

        TreeNode<Link> rootNode = importNodeFromFile("io/wiki/wiki-example-3.txt");
        Note root = createAtom("0000000", "root");
        root.setSource(DefaultSources.UNIVERSAL);
        TreeViews.setId(rootNode, root.getId());
        queries.update(rootNode, 2, writeFilter, style);

        Optional<Note> a1 = topicGraph.getNotesById("0000001");
        assertEquals(DefaultSources.UNIVERSAL, a1.get().getSource());
        Optional<Note> a2 = topicGraph.getNotesById("0000002");
        assertEquals(DefaultSources.PERSONAL, a2.get().getSource());

        TreeNode<Link> after = queries.view(root, 2, readFilter, style);
        // the second atom in the list simply does not appear
        Assert.assertEquals(2, after.getNumberOfChildren());
        Assert.assertEquals(0, after.getNumberOfParents());
        ListNode<TreeNode<Link>> children = after.getChildren();
        TreeNode<Link> n1 = children.get(0);
        Assert.assertEquals(1, n1.getNumberOfChildren());
        Assert.assertEquals(1, n1.getNumberOfParents());

        TreeNode<Link> n5 = children.get(1);
        // children are not visible
        Assert.assertEquals(0, n5.getNumberOfChildren());
        Assert.assertEquals(1, n5.getNumberOfParents());

        ListNode<TreeNode<Link>> grandChildren = n1.getChildren();
        assertEquals(1, grandChildren.length());
        TreeNode<Link> n3 = grandChildren.get(0);
        Assert.assertEquals(0, n3.getNumberOfChildren());
        Assert.assertEquals(1, n3.getNumberOfParents());
        assertEquals("this is a public child of a public note", TreeViews.getTitle(n3));
    }

    @Test
    public void invisibleAtomsAreSkippedDuringWrites() throws Exception {
        Filter readFilter = Filter.noFilter();
        Filter writeFilter = new Filter(0f, 0.5f, DefaultSources.PUBLIC, DefaultSources.PUBLIC);
        ViewStyle style = ViewStyle.Basic.Forward.getStyle();

        // three items in the private view
        TreeNode<Link> rootNode = importNodeFromFile("io/wiki/wiki-example-3.txt");
        Note root = createAtom("0000000", "node 0");
        root.setSource(DefaultSources.UNIVERSAL);
        TreeViews.setId(rootNode, root.getId());
        queries.update(rootNode, 2, Filter.noFilter(), style);
        rootNode = queries.view(root, 2, readFilter, style);
        assertEquals(3, rootNode.getNumberOfChildren());
        assertChildCount(3, rootNode);
        assertEquals(DefaultSources.PERSONAL, TreeViews.getSource(rootNode.getChildren().get(1)));

        // add an atom in the public view
        rootNode = queries.view(root, 2, writeFilter, style);
        TreeNode<Link> toAdd = createTree();
        TreeViews.setId(toAdd, "0000007");
        TreeViews.setTitle(toAdd, "node 7");
        TreeViews.setSource(toAdd, DefaultSources.UNIVERSAL);
        rootNode.setChildren(rootNode.getChildren().add(0, toAdd));
        queries.update(rootNode, 2, writeFilter, style);

        // four total items in the private view
        rootNode = queries.view(root, 2, readFilter, style);
        assertChildCount(4, rootNode);
        assertEquals("node 7", TreeViews.getTitle(rootNode.getChildren().get(0)));
        rootNode = queries.view(root, 2, writeFilter, style);
        assertChildCount(3, rootNode);
        assertEquals("node 7", TreeViews.getTitle(rootNode.getChildren().get(0)));
        rootNode.getChildren().remove(2);
        queries.update(rootNode, 2, writeFilter, style);

        rootNode = queries.view(root, 2, readFilter, style);
        assertChildCount(3, rootNode);
        assertEquals("node 7", TreeViews.getTitle(rootNode.getChildren().get(0)));
        assertEquals("this is a public note", TreeViews.getTitle(rootNode.getChildren().get(1)));
        assertEquals("this is a protected note", TreeViews.getTitle(rootNode.getChildren().get(2)));
        rootNode = queries.view(root, 2, writeFilter, style);
        assertChildCount(2, rootNode);
        assertEquals("node 7", TreeViews.getTitle(rootNode.getChildren().get(0)));
        assertEquals("this is a public note", TreeViews.getTitle(rootNode.getChildren().get(1)));

        toAdd = createTree();
        TreeViews.setId(toAdd, "0000008");
        TreeViews.setTitle(toAdd, "node 8");
        TreeViews.setSource(toAdd, DefaultSources.UNIVERSAL);
        rootNode.getChildren().add(2, toAdd);
        queries.update(rootNode, 2, writeFilter, style);

        rootNode = queries.view(root, 2, readFilter, style);
        assertChildCount(4, rootNode);
        assertEquals("node 7", TreeViews.getTitle(rootNode.getChildren().get(0)));
        assertEquals("this is a public note", TreeViews.getTitle(rootNode.getChildren().get(1)));
        assertEquals("this is a protected note", TreeViews.getTitle(rootNode.getChildren().get(2)));
        assertEquals("node 8", TreeViews.getTitle(rootNode.getChildren().get(3)));
        rootNode = queries.view(root, 2, writeFilter, style);
        assertChildCount(3, rootNode);
        assertEquals("node 7", TreeViews.getTitle(rootNode.getChildren().get(0)));
        assertEquals("this is a public note", TreeViews.getTitle(rootNode.getChildren().get(1)));
        assertEquals("node 8", TreeViews.getTitle(rootNode.getChildren().get(2)));
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

        TreeNode<Link> b = parseToTree(before);
        TreeNode<Link> a = parseToTree(after);

        // First, check that 'after' was parsed correctly
        assertChildCount(3, a);
        assertEquals("000002", TreeViews.getId(a.getChildren().get(1)));
        assertNull(TreeViews.getTitle(a.getChildren().get(1)));

        Note root = createAtom("000000", "root");

        TreeViews.setId(b, root.getId());
        queries.update(b, 2, filter, style);

        Note a1 = topicGraph.getNotesById("000001").get();
        Note a2 = topicGraph.getNotesById("000002").get();
        Note a3 = topicGraph.getNotesById("000003").get();

        assertEquals("one", a1.getTitle());
        assertEquals("two", a2.getTitle());
        assertEquals("three", a3.getTitle());

        TreeViews.setId(a, root.getId());
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

        TreeNode<Link> b = parseToTree(before);
        TreeNode<Link> a = parseToTree(after);

        Note root = createAtom("000000", "root");

        TreeViews.setId(b, root.getId());
        queries.update(b, 2, filter, style);

        Note a1 = topicGraph.getNotesById("000001").get();
        Note a2 = topicGraph.getNotesById("000002").get();
        Note a3 = topicGraph.getNotesById("000003").get();

        assertEquals("one", a1.getTitle());
        assertEquals("two", a2.getTitle());
        assertEquals("three", a3.getTitle());
        assertEquals(3, ListNode.toJavaList(root.getChildren()).size());

        TreeViews.setId(a, root.getId());
        queries.update(a, 2, filter, style);

        Note a4 = topicGraph.getNotesById("000004").get();

        assertEquals("four", a4.getTitle());
        List<Note> children = ListNode.toJavaList(root.getChildren());
        assertEquals(4, children.size());
        assertEquals("four", children.get(0).getTitle());
        assertEquals("one", children.get(1).getTitle());
        assertEquals("two", children.get(2).getTitle());
    }

    @Test
    public void testFindRootsAndIsolatedAtoms() throws Exception {
        assertChildCount(0, queries.findRootAtoms(filter, ViewStyle.Basic.Forward.getStyle(), 1));
        assertChildCount(0, queries.findRootAtoms(filter, ViewStyle.Basic.Backward.getStyle(), 1));
        assertChildCount(0, queries.findIsolatedAtoms(filter));

        Note atom0 = createAtom("000000", "0");

        assertChildCount(1, queries.findRootAtoms(filter, ViewStyle.Basic.Forward.getStyle(), 1));
        assertChildCount(1, queries.findRootAtoms(filter, ViewStyle.Basic.Backward.getStyle(), 1));
        assertChildCount(1, queries.findIsolatedAtoms(filter));

        Note atom1 = createAtom("000001", "1");

        assertChildCount(2, queries.findRootAtoms(filter, ViewStyle.Basic.Forward.getStyle(), 1));
        assertChildCount(2, queries.findRootAtoms(filter, ViewStyle.Basic.Backward.getStyle(), 1));
        assertChildCount(2, queries.findIsolatedAtoms(filter));

        atom0.addChildAt(atom1, 0);

        assertChildCount(1, queries.findRootAtoms(filter, ViewStyle.Basic.Forward.getStyle(), 1));
        assertChildCount(1, queries.findRootAtoms(filter, ViewStyle.Basic.Backward.getStyle(), 1));
        assertChildCount(0, queries.findIsolatedAtoms(filter));

        Note atom2 = createAtom("000002", "2");

        assertChildCount(2, queries.findRootAtoms(filter, ViewStyle.Basic.Forward.getStyle(), 1));
        assertChildCount(2, queries.findRootAtoms(filter, ViewStyle.Basic.Backward.getStyle(), 1));
        assertChildCount(1, queries.findIsolatedAtoms(filter));

        atom0.addChildAt(atom2, 0);

        assertChildCount(1, queries.findRootAtoms(filter, ViewStyle.Basic.Forward.getStyle(), 1));
        assertChildCount(2, queries.findRootAtoms(filter, ViewStyle.Basic.Backward.getStyle(), 1));
        assertChildCount(0, queries.findIsolatedAtoms(filter));
    }

    private TreeNode<Link> createTree() {
        TreeNode<Link> node = new TreeNodeDTO<>();
        Link link = new LinkDTO();
        link.setPage(new PageDTO());
        node.setValue(link);
        return node;
    }
}
