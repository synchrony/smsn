package net.fortytwo.smsn.brain;

import net.fortytwo.smsn.brain.model.Note;
import net.fortytwo.smsn.brain.model.TopicGraph;
import net.fortytwo.smsn.brain.query.TreeViews;
import net.fortytwo.smsn.brain.query.ViewStyle;
import org.junit.Test;

import java.io.IOException;

import static junit.framework.TestCase.assertEquals;

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

    private Note search(final String query) {
        return queries.search(TreeViews.QueryType.FullText, query, 1, filter, ViewStyle.Basic.Forward.getStyle());
    }
}
