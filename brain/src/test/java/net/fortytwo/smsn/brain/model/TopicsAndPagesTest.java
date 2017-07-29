package net.fortytwo.smsn.brain.model;

import net.fortytwo.smsn.brain.BrainTestBase;
import net.fortytwo.smsn.brain.model.entities.ListNode;
import net.fortytwo.smsn.brain.model.entities.TreeNode;
import net.fortytwo.smsn.brain.model.entities.Link;
import net.fortytwo.smsn.brain.model.entities.Page;
import net.fortytwo.smsn.brain.model.pg.PGPage;
import net.fortytwo.smsn.brain.model.pg.PGTopic;
import org.junit.Before;
import org.junit.Test;

import java.io.IOException;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertFalse;
import static org.junit.Assert.assertNull;
import static org.junit.Assert.assertTrue;

public class TopicsAndPagesTest extends BrainTestBase {

    private Link friendsLink, arthurLink, fordLink, zaphodLink, earthLink, teaLink;

    @Override
    protected TopicGraph createTopicGraph() throws IOException {
        return createNeo4jTopicGraph();
    }

    @Before
    @Override
    public void setUp() throws Exception {
        super.setUp();

        friendsLink = topicGraph.createLink(friendTopic, "friends", Role.Relation);
        arthurLink = topicGraph.createLink(arthurTopic, "Arthur P. Dent", null);
        fordLink = topicGraph.createLink(fordTopic, "Ford Prefect", null);
        zaphodLink = topicGraph.createLink(zaphodTopic, "Zaphod Beeblebrox", Role.Relation);
        earthLink = topicGraph.createLink(earthTopic, "The Earth", null);
        teaLink = topicGraph.createLink(teaTopic, "Tea", null);
    }

    @Test
    public void ordinaryPageAndTopicTreeAreAllowed() {
        Page page = topicGraph.createPage(arthurLink);
        page.setText("a page about Arthur");
        TreeNode<Link> tree = page.getContent();
        tree.setValue(arthurLink);
        tree.setChildren(topicGraph.toList(
                topicGraph.createTopicTree(teaLink)));
        assertEquals("a page about Arthur", page.getText());
        assertEquals("Arthur P. Dent", page.getContent().getValue().getLabel());

        Link secondLink = page.getContent().getChildren().get(0).getValue();
        assertEquals("Tea", secondLink.getLabel());
        assertEquals("tea", secondLink.getTarget().getId());

        TreeNode<Link> friendsTree = topicGraph.createTopicTree(friendsLink);
        ListNode<TreeNode<Link>> childLinks = topicGraph.toList(
                topicGraph.createTopicTree(fordLink),
                topicGraph.createTopicTree(zaphodLink));
        friendsTree.setChildren(childLinks);
        ListNode<TreeNode<Link>> children = topicGraph.toList(friendsTree);
        tree.setChildren(children);

        assertEquals("friends", page.getContent().getChildren().get(0).getValue().getLabel());
        assertEquals("Ford Prefect",
                page.getContent().getChildren().get(0).getChildren().get(0).getValue().getLabel());

        page.destroy();
    }

    @Test(expected = IllegalArgumentException.class)
    public void linkWithoutTargetIsNotAllowed() {
        topicGraph.createLink(null, "nowhere", null);
    }

    @Test(expected = IllegalArgumentException.class)
    public void linkWithoutLabelIsNotAllowed() {
        topicGraph.createLink(arthurTopic, null, null);
    }

    @Test(expected = IllegalArgumentException.class)
    public void settingPageTreeToNullIsNotAllowed() {
        Page page = topicGraph.createPage(arthurLink);
        page.setText("a page about Arthur");
        page.setContent(null);
    }

    @Test(expected = NullPointerException.class)
    public void treeWithoutValueIsNotAllowed() {
        topicGraph.createTopicTree(null);
    }

    @Test(expected = IllegalArgumentException.class)
    public void emptyListOfLinksIsNotAllowed() {
        topicGraph.toList(new Link[]{});
    }

    @Test(expected = IllegalArgumentException.class)
    public void emptyListOfSubtreesIsNotAllowed() {
        topicGraph.toList(new TreeNode[]{});
    }

    @Test(expected = IllegalArgumentException.class)
    public void settingTreeTopicToNullIsNotAllowed() {
        TreeNode<Link> tree = topicGraph.createTopicTree(arthurLink);
        tree.setValue(null);
    }

    @Test
    public void topicDAGIsDestroyedGracefully() {
        Page page = topicGraph.createPage(arthurLink);

        Object arthurId = ((PGTopic) arthurTopic).asVertex().id();
        assertTrue(graph.traversal().V(arthurId).hasNext());
        Object pageId = ((PGPage) page).asVertex().id();
        assertTrue(graph.traversal().V(pageId).hasNext());

        page.getContent().setChildren(
                topicGraph.toList(
                        topicGraph.createTopicTree(
                                topicGraph.createLink(arthurTopic, "Arthur as a top-level link", null)),
                        topicGraph.createTopicTree(
                                topicGraph.createLink(arthurTopic, "Arthur as a header", Role.Relation))));
        page.getContent().getChildren().get(1).setChildren(
                topicGraph.toList(
                        topicGraph.createTopicTree(
                                topicGraph.createLink(arthurTopic, "Arthur as a second-level link", null))));

        assertEquals(arthurTopic, page.getContent().getValue().getTarget());
        assertEquals("Arthur P. Dent", page.getContent().getValue().getLabel());
        assertNull(page.getContent().getValue().getRole());
        assertEquals(arthurTopic, page.getContent().getChildren().get(0).getValue().getTarget());
        assertEquals("Arthur as a top-level link", page.getContent().getChildren().get(0).getValue().getLabel());
        assertNull(page.getContent().getChildren().get(0).getValue().getRole());
        assertEquals(arthurTopic, page.getContent().getChildren().get(1).getValue().getTarget());
        assertEquals("Arthur as a header", page.getContent().getChildren().get(1).getValue().getLabel());
        assertEquals(Role.Relation, page.getContent().getChildren().get(1).getValue().getRole());
        assertEquals(arthurTopic, page.getContent().getChildren().get(1).getChildren().get(0).getValue().getTarget());
        assertEquals("Arthur as a second-level link",
                page.getContent().getChildren().get(1).getChildren().get(0).getValue().getLabel());
        assertNull(page.getContent().getChildren().get(1).getChildren().get(0).getValue().getRole());

        // before destroying the page, create another page about Arthur
        Page page2 = topicGraph.createPage(topicGraph.createLink(arthurTopic, "Arthur Philip Dent", null));
        Object page2Id = ((PGPage) page2).asVertex().id();

        assertTrue(graph.traversal().V(arthurId).hasNext());
        assertTrue(graph.traversal().V(pageId).hasNext());
        assertTrue(graph.traversal().V(page2Id).hasNext());

        page.destroy();

        // Arthur is still here, as it is also referenced by page2
        assertTrue(graph.traversal().V(arthurId).hasNext());
        assertFalse(graph.traversal().V(pageId).hasNext());
        assertTrue(graph.traversal().V(page2Id).hasNext());

        page2.destroy();

        assertFalse(graph.traversal().V(arthurId).hasNext());
        assertFalse(graph.traversal().V(pageId).hasNext());
        assertFalse(graph.traversal().V(page2Id).hasNext());
    }
}