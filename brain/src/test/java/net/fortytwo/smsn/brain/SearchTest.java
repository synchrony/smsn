package net.fortytwo.smsn.brain;

import net.fortytwo.smsn.brain.model.Note;
import net.fortytwo.smsn.brain.model.TopicGraph;
import net.fortytwo.smsn.brain.model.entities.Atom;
import net.fortytwo.smsn.brain.query.TreeViews;
import net.fortytwo.smsn.brain.query.ViewStyle;
import org.junit.Test;

import java.io.IOException;
import java.util.Random;

import static junit.framework.TestCase.assertEquals;
import static org.junit.Assert.assertTrue;

public class SearchTest extends BrainTestBase {

    @Override
    protected TopicGraph createAtomGraph() throws IOException {
        return createNeo4jAtomGraph();
    }

    @Test
    public void emptyGraphHasNoSearchResults() throws Exception {
        assertEquals(0, search("nothing").getChildren().size());
    }

    @Test
    public void partialMatchSucceeds() throws Exception {
        createAtom(ARTHUR_ID, "Arthur Dent");
        createAtom(FORD_ID, "Ford");

        Note results = search("Arthur");
        assertEquals(1, results.getChildren().size());
        assertEquals(ARTHUR_ID, results.getChildren().get(0).getId());
    }

    @Test
    public void spaceSeparatedTokensHaveAndSemantics() throws Exception {
        createAtom(ARTHUR_ID, "Arthur Dent");
        createAtom("Random Frequent Flyer Dent");

        assertEquals(1, search("Arthur").getChildren().size());
        assertEquals(1, search("Flyer").getChildren().size());
        assertEquals(2, search("Dent").getChildren().size());

        assertEquals(2, search("Arthur OR Dent").getChildren().size());
        assertEquals(1, search("Arthur AND Dent").getChildren().size());
        assertEquals(1, search("Arthur Dent").getChildren().size());
    }

    @Test
    public void moreExactMatchScoresHigher() throws Exception {
        createAtom("Arthur");
        createAtom("Arthur Dent");

        Note results = search("arthur");
        assertEquals(2, results.getChildren().size());
        assertEquals("Arthur", results.getChildren().get(0).getTitle());
        assertEquals("Arthur Dent", results.getChildren().get(1).getTitle());
    }

    @Test
    public void higherWeightScoresHigher() throws Exception {
        Random random = new Random();
        for (int i = 0; i < 10; i++) {
            Atom a = createAtom("Lintilla");
            a.setWeight(random.nextFloat());
        }

        Note results = search("Lintilla");
        assertEquals(10, results.getChildren().size());
        float lastWeight = 1f;
        for (Note result : results.getChildren()) {
            assertEquals("Lintilla", result.getTitle());
            float weight = result.getWeight();
            assertTrue(weight <= lastWeight);
            lastWeight = weight;
        }
    }

    @Test
    public void higherPriorityScoresHigher() throws Exception {
        Random random = new Random();
        for (int i = 0; i < 10; i++) {
            Atom a = createAtom("Lintilla");
            if (random.nextBoolean()) {
                a.setPriority(random.nextFloat());
            }
        }

        Note results = search("Lintilla");
        assertEquals(10, results.getChildren().size());
        float lastPriority = 1f;
        for (Note result : results.getChildren()) {
            assertEquals("Lintilla", result.getTitle());
            float priority = null == result.getPriority() ? 0f : result.getPriority();
            assertTrue(priority <= lastPriority);
            lastPriority = priority;
        }
    }

    private Note search(final String query) {
        return queries.search(TreeViews.QueryType.FullText, query, 1, filter, ViewStyle.Basic.Forward.getStyle());
    }
}
