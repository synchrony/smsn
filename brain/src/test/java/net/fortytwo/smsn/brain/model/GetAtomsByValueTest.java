package net.fortytwo.smsn.brain.model;

import net.fortytwo.smsn.brain.BrainTestBase;
import net.fortytwo.smsn.brain.model.entities.Note;
import org.junit.Before;
import org.junit.Test;

import java.io.IOException;

import static org.junit.Assert.assertEquals;

public class GetAtomsByValueTest extends BrainTestBase {
    private Note arthur;

    @Override
    protected TopicGraph createTopicGraph() throws IOException {
        return createNeo4jTopicGraph();
    }

    @Before
    @Override
    public void setUp() throws Exception {
        super.setUp();

        arthur = createNote();
        arthur.setTitle("Arthur Dent");
    }

    @Test
    public void nonMatchingValueFails() throws Exception {
        result = topicGraph.getNotesByTitleQuery("Zaphod", filter);
        assertEquals(0, result.size());
    }

    @Test
    public void completelyMatchingValueSucceeds() throws Exception {
        result = topicGraph.getNotesByTitleQuery("Arthur Dent", filter);
        assertEquals(1, result.size());
    }

    @Test
    public void subsetOfWordsMatchingSucceeds() throws Exception {
        result = topicGraph.getNotesByTitleQuery("Arthur Beeblebrox", filter);
        assertEquals(1, result.size());
    }

    @Test
    public void outOfOrderWordsSucceeds() throws Exception {
        result = topicGraph.getNotesByTitleQuery("Dent Arthur", filter);
        assertEquals(1, result.size());
    }

    @Test
    public void partialValueSucceeds() throws Exception {
        result = topicGraph.getNotesByTitleQuery("Arthur", filter);
        assertEquals(1, result.size());
    }

    @Test
    public void partialWordFails() throws Exception {
        result = topicGraph.getNotesByTitleQuery("Arth", filter);
        assertEquals(0, result.size());
    }

    @Test
    public void matchingWildcardSucceeds() throws Exception {
        result = topicGraph.getNotesByTitleQuery("Arth*", filter);
        assertEquals(1, result.size());
    }

    @Test
    public void nonMatchingWildcardFails() throws Exception {
        result = topicGraph.getNotesByTitleQuery("Zaph*", filter);
        assertEquals(0, result.size());
    }

    @Test
    public void caseInsensitiveMatchSucceeds() throws Exception {
        result = topicGraph.getNotesByTitleQuery("ARTHUR Dent", filter);
        assertEquals(1, result.size());
        result = topicGraph.getNotesByTitleQuery("aRTHur", filter);
        assertEquals(1, result.size());
        result = topicGraph.getNotesByTitleQuery("*dENT", filter);
        assertEquals(1, result.size());
    }

    @Test
    public void quotedExactMatchSucceeds() throws Exception {
        result = topicGraph.getNotesByTitleQuery("\"Arthur Dent\"", filter);
        assertEquals(1, result.size());
        assertEquals(arthur.getId(), result.iterator().next().getId());
    }
}
