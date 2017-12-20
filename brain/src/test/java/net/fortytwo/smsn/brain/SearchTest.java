package net.fortytwo.smsn.brain;

import net.fortytwo.smsn.brain.model.TopicGraph;
import net.fortytwo.smsn.brain.model.entities.ListNode;
import net.fortytwo.smsn.brain.model.entities.Note;
import net.fortytwo.smsn.brain.query.Model;
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
        createNote(ARTHUR_ID, "Arthur Dent");
        createNote(FORD_ID, "Ford");

        Note results = search("Arthur");
        assertChildCount(1, results);
        assertEquals(ARTHUR_ID, Model.getTopicId(results.getFirst().get(0)));
    }

    @Test
    public void spaceSeparatedTokensHaveAndSemantics() throws Exception {
        createNote(ARTHUR_ID, "Arthur Dent");
        createNoteWithTitle("Random Frequent Flyer Dent");

        assertChildCount(1, search("Arthur"));
        assertChildCount(1, search("Flyer"));
        assertChildCount(2, search("Dent"));

        assertChildCount(2, search("Arthur OR Dent"));
        assertChildCount(1, search("Arthur AND Dent"));
        assertChildCount(1, search("Arthur Dent"));
    }

    @Test
    public void moreExactMatchScoresHigher() throws Exception {
        createNoteWithTitle("Arthur");
        createNoteWithTitle("Arthur Dent");

        Note results = search("arthur");
        assertChildCount(2, results);
        assertEquals("Arthur", results.getFirst().get(0).getLabel());
        assertEquals("Arthur Dent", results.getFirst().get(1).getLabel());
    }

    @Test
    public void higherWeightScoresHigher() throws Exception {
        Random random = new Random();
        for (int i = 0; i < 10; i++) {
            Note a = createNoteWithTitle("Lintilla");
            a.setWeight(random.nextFloat());
        }

        Note results = search("Lintilla");
        assertChildCount(10, results);
        float lastWeight = 1f;
        for (Note result : ListNode.toJavaList(results.getFirst())) {
            assertEquals("Lintilla", result.getLabel());
            float weight = result.getWeight();
            assertTrue(weight <= lastWeight);
            lastWeight = weight;
        }
    }

    @Test
    public void higherPriorityScoresHigher() throws Exception {
        Random random = new Random();
        for (int i = 0; i < 10; i++) {
            Note a = createNoteWithTitle("Lintilla");
            if (random.nextBoolean()) {
                a.setPriority(random.nextFloat());
            }
        }

        Note results = search("Lintilla");
        assertChildCount(10, results);
        float lastPriority = 1f;
        for (Note result : ListNode.toJavaList(results.getFirst())) {
            assertEquals("Lintilla", result.getLabel());
            float priority = null == result.getPriority() ? 0f : result.getPriority();
            assertTrue(priority <= lastPriority);
            lastPriority = priority;
        }
    }

    private Note search(final String query) {
        return model.search(Model.QueryType.FullText, query, 1, filter, ViewStyle.Basic.Forward.getStyle());
    }
}
