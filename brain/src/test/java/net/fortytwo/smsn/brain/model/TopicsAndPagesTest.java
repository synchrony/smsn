package net.fortytwo.smsn.brain.model;

import net.fortytwo.smsn.brain.BrainTestBase;
import net.fortytwo.smsn.brain.model.entities.EntityList;
import net.fortytwo.smsn.brain.model.entities.KeyValueTree;
import net.fortytwo.smsn.brain.model.entities.Link;
import net.fortytwo.smsn.brain.model.entities.Page;
import net.fortytwo.smsn.brain.model.entities.Topic;
import net.fortytwo.smsn.brain.model.pg.PGPage;
import net.fortytwo.smsn.brain.model.pg.PGTopic;
import org.junit.Before;
import org.junit.Test;

import java.io.IOException;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertFalse;
import static org.junit.Assert.assertTrue;

public class TopicsAndPagesTest extends BrainTestBase {

    private Topic arthurTopic, fordTopic, zaphodTopic, friendTopic, earthTopic, teaTopic;
    private Link friendsLink, arthurLink, fordLink, zaphodLink, earthLink, teaLink;

    @Override
    protected TopicGraph createAtomGraph() throws IOException {
        return createNeo4jAtomGraph();
    }

    @Before
    @Override
    public void setUp() throws Exception {
        super.setUp();

        arthurTopic = topicGraph.createTopic("arthur");
        fordTopic = topicGraph.createTopic("ford");
        zaphodTopic = topicGraph.createTopic("zaphod");
        friendTopic = topicGraph.createTopic("friend");
        earthTopic = topicGraph.createTopic("earth");
        teaTopic = topicGraph.createTopic("tea");

        friendsLink = topicGraph.createLink(friendTopic, "friends");
        arthurLink = topicGraph.createLink(arthurTopic, "Arthur P. Dent");
        fordLink = topicGraph.createLink(fordTopic, "Ford Prefect");
        zaphodLink = topicGraph.createLink(zaphodTopic, "Zaphod Beeblebrox");
        earthLink = topicGraph.createLink(earthTopic, "The Earth");
        teaLink = topicGraph.createLink(teaTopic, "Tea");
    }

    @Test
    public void ordinaryPageAndTopicTreeAreAllowed() {
        Page page = topicGraph.createPage(arthurLink);
        page.setText("a page about Arthur");

        assertEquals("a page about Arthur", page.getText());
        assertEquals("Arthur P. Dent", page.getTopicTree().getKey().getLabel());

        KeyValueTree<Link, EntityList<Link>> tree = page.getTopicTree();
        EntityList<Link> parentLinks = topicGraph.createListOfLinks(earthLink, teaLink);
        tree.setValue(parentLinks);

        Link secondLink = page.getTopicTree().getValue().toJavaList().get(1);
        assertEquals("Tea", secondLink.getLabel());
        assertEquals("tea", secondLink.getTarget().getId());

        KeyValueTree<Link, EntityList<Link>> friendsTree = topicGraph.createTopicTree(friendsLink);
        EntityList<Link> childLinks = topicGraph.createListOfLinks(fordLink, zaphodLink);
        friendsTree.setValue(childLinks);
        EntityList<KeyValueTree<Link, EntityList<Link>>> children = topicGraph.createListOfTrees(friendsTree);
        tree.setChildren(children);

        assertEquals("friends", page.getTopicTree().getChildren().getFirst().getKey().getLabel());
        assertEquals("Ford Prefect", page.getTopicTree().getChildren().getFirst().getValue().getFirst().getLabel());

        page.destroy();

    }

    @Test(expected = IllegalArgumentException.class)
    public void linkWithoutTargetIsNotAllowed() {
        topicGraph.createLink(null, "nowhere");
    }

    @Test(expected = IllegalArgumentException.class)
    public void linkWithoutLabelIsNotAllowed() {
        topicGraph.createLink(arthurTopic, null);
    }

    @Test(expected = IllegalArgumentException.class)
    public void settingPageTreeToNullIsNotAllowed() {
        Page page = topicGraph.createPage(arthurLink);
        page.setText("a page about Arthur");
        page.setTopicTree(null);
    }

    @Test(expected = IllegalArgumentException.class)
    public void treeWithoutKeyIsNotAllowed() {
        topicGraph.createTopicTree(null);
    }

    @Test(expected = IllegalArgumentException.class)
    public void emptyListOfLinksIsNotAllowed() {
        topicGraph.createListOfLinks();
    }

    @Test(expected = IllegalArgumentException.class)
    public void emptyListOfSubtreesIsNotAllowed() {
        topicGraph.createListOfTrees();
    }

    @Test
    public void treeWithoutChildrenOrLinksIsAllowed() {
        Page page = topicGraph.createPage(arthurLink);
        KeyValueTree<Link, EntityList<Link>> tree = page.getTopicTree();

        tree.setChildren(null);
        tree.setValue(null);

        page.destroy();
    }

    @Test(expected = IllegalArgumentException.class)
    public void settingTreeTopicToNullIsNotAllowed() {
        KeyValueTree<Link, EntityList<Link>> tree = topicGraph.createTopicTree(arthurLink);
        tree.setKey(null);
    }

    @Test
    public void topicDAGIsDestroyedGracefully() {
        Page page = topicGraph.createPage(arthurLink);

        Object arthurId = ((PGTopic) arthurTopic).asVertex().id();
        assertTrue(graph.traversal().V(arthurId).hasNext());
        Object pageId = ((PGPage) page).asVertex().id();
        assertTrue(graph.traversal().V(pageId).hasNext());

        page.getTopicTree().setValue(
                topicGraph.createListOfLinks(
                        topicGraph.createLink(arthurTopic, "Arthur as a top-level link")));

        page.getTopicTree().setChildren(
                topicGraph.createListOfTrees(
                        topicGraph.createTopicTree(
                                topicGraph.createLink(arthurTopic, "Arthur as a header"))));
        page.getTopicTree().getChildren().getFirst().setValue(
                topicGraph.createListOfLinks(
                        topicGraph.createLink(arthurTopic, "Arthur as a second-level link")));

        assertEquals(arthurTopic, page.getTopicTree().getKey().getTarget());
        assertEquals("Arthur P. Dent", page.getTopicTree().getKey().getLabel());
        assertEquals(arthurTopic, page.getTopicTree().getValue().getFirst().getTarget());
        assertEquals("Arthur as a top-level link", page.getTopicTree().getValue().getFirst().getLabel());
        assertEquals(arthurTopic, page.getTopicTree().getChildren().getFirst().getKey().getTarget());
        assertEquals("Arthur as a header", page.getTopicTree().getChildren().getFirst().getKey().getLabel());
        assertEquals(arthurTopic, page.getTopicTree().getChildren().getFirst().getValue().getFirst().getTarget());
        assertEquals("Arthur as a second-level link",
                page.getTopicTree().getChildren().getFirst().getValue().getFirst().getLabel());

        // before destroying the page, create another page about Arthur
        Page page2 = topicGraph.createPage(topicGraph.createLink(arthurTopic, "Arthur Philip Dent"));
        Object page2Id = ((PGPage) page2).asVertex().id();

        assertTrue(graph.traversal().V(arthurId).hasNext());
        assertTrue(graph.traversal().V(pageId).hasNext());
        assertTrue(graph.traversal().V(page2Id).hasNext());

        page.destroy();

        assertTrue(graph.traversal().V(arthurId).hasNext());
        assertFalse(graph.traversal().V(pageId).hasNext());
        assertTrue(graph.traversal().V(page2Id).hasNext());

        page2.destroy();

        assertFalse(graph.traversal().V(arthurId).hasNext());
        assertFalse(graph.traversal().V(pageId).hasNext());
        assertFalse(graph.traversal().V(page2Id).hasNext());
    }
}