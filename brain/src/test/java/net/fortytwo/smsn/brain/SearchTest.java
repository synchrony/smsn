package net.fortytwo.smsn.brain;

import net.fortytwo.smsn.brain.model.TopicGraph;
import net.fortytwo.smsn.brain.model.entities.Note;
import net.fortytwo.smsn.brain.model.entities.Link;
import net.fortytwo.smsn.brain.model.entities.ListNode;
import net.fortytwo.smsn.brain.model.entities.TreeNode;
import net.fortytwo.smsn.brain.query.TreeViews;
import net.fortytwo.smsn.brain.query.ViewStyle;
import org.junit.Test;

import java.io.IOException;
import java.util.Random;

import static junit.framework.TestCase.assertEquals;
import static org.junit.Assert.assertTrue;

public class SearchTest extends BrainTestBase {

    @Override
    protected TopicGraph createTopicGraph() throws IOException {
        return createNeo4jTopicGraph();
    }

    @Test
    public void emptyGraphHasNoSearchResults() throws Exception {
        assertChildCount(0, search("nothing"));
    }

    @Test
    public void partialMatchSucceeds() throws Exception {
        createAtom(ARTHUR_ID, "Arthur Dent");
        createAtom(FORD_ID, "Ford");

        TreeNode<Link> results = search("Arthur");
        assertChildCount(1, results);
        assertEquals(ARTHUR_ID, TreeViews.getId(results.getChildren().get(0)));
    }

    @Test
    public void spaceSeparatedTokensHaveAndSemantics() throws Exception {
        createAtom(ARTHUR_ID, "Arthur Dent");
        createAtom("Random Frequent Flyer Dent");

        assertChildCount(1, search("Arthur"));
        assertChildCount(1, search("Flyer"));
        assertChildCount(2, search("Dent"));

        assertChildCount(2, search("Arthur OR Dent"));
        assertChildCount(1, search("Arthur AND Dent"));
        assertChildCount(1, search("Arthur Dent"));
    }

    @Test
    public void moreExactMatchScoresHigher() throws Exception {
        createAtom("Arthur");
        createAtom("Arthur Dent");

        TreeNode<Link> results = search("arthur");
        assertChildCount(2, results);
        assertEquals("Arthur", TreeViews.getTitle(results.getChildren().get(0)));
        assertEquals("Arthur Dent", TreeViews.getTitle(results.getChildren().get(1)));
    }

    @Test
    public void higherWeightScoresHigher() throws Exception {
        Random random = new Random();
        for (int i = 0; i < 10; i++) {
            Note a = createAtom("Lintilla");
            a.setWeight(random.nextFloat());
        }

        TreeNode<Link> results = search("Lintilla");
        assertChildCount(10, results);
        float lastWeight = 1f;
        for (TreeNode<Link> result : ListNode.toJavaList(results.getChildren())) {
            assertEquals("Lintilla", TreeViews.getTitle(result));
            float weight = TreeViews.getWeight(result);
            assertTrue(weight <= lastWeight);
            lastWeight = weight;
        }
    }

    @Test
    public void higherPriorityScoresHigher() throws Exception {
        Random random = new Random();
        for (int i = 0; i < 10; i++) {
            Note a = createAtom("Lintilla");
            if (random.nextBoolean()) {
                a.setPriority(random.nextFloat());
            }
        }

        TreeNode<Link> results = search("Lintilla");
        assertChildCount(10, results);
        float lastPriority = 1f;
        for (TreeNode<Link> result : ListNode.toJavaList(results.getChildren())) {
            assertEquals("Lintilla", TreeViews.getTitle(result));
            float priority = null == TreeViews.getPriority(result) ? 0f : TreeViews.getPriority(result);
            assertTrue(priority <= lastPriority);
            lastPriority = priority;
        }
    }

    private TreeNode<Link> search(final String query) {
        return queries.search(TreeViews.QueryType.FullText, query, 1, filter, ViewStyle.Basic.Forward.getStyle());
    }
}
