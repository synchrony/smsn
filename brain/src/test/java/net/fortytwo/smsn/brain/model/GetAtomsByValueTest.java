package net.fortytwo.smsn.brain.model;

import net.fortytwo.smsn.brain.BrainTestBase;
import net.fortytwo.smsn.brain.model.entities.Atom;
import org.junit.Before;
import org.junit.Test;

import java.io.IOException;

import static org.junit.Assert.assertEquals;

public class GetAtomsByValueTest extends BrainTestBase {
    private Atom arthur;

    @Override
    protected TopicGraph createTopicGraph() throws IOException {
        return createNeo4jTopicGraph();
    }

    @Before
    @Override
    public void setUp() throws Exception {
        super.setUp();

        arthur = topicGraph.createAtomWithProperties(filter, null);
        arthur.setTitle("Arthur Dent");
    }

    @Test
    public void nonMatchingValueFails() throws Exception {
        queryResult = topicGraph.getAtomsByTitleQuery("Zaphod", filter);
        assertEquals(0, queryResult.size());
    }

    @Test
    public void completelyMatchingValueSucceeds() throws Exception {
        queryResult = topicGraph.getAtomsByTitleQuery("Arthur Dent", filter);
        assertEquals(1, queryResult.size());
    }

    @Test
    public void subsetOfWordsMatchingSucceeds() throws Exception {
        queryResult = topicGraph.getAtomsByTitleQuery("Arthur Beeblebrox", filter);
        assertEquals(1, queryResult.size());
    }

    @Test
    public void outOfOrderWordsSucceeds() throws Exception {
        queryResult = topicGraph.getAtomsByTitleQuery("Dent Arthur", filter);
        assertEquals(1, queryResult.size());
    }

    @Test
    public void partialValueSucceeds() throws Exception {
        queryResult = topicGraph.getAtomsByTitleQuery("Arthur", filter);
        assertEquals(1, queryResult.size());
    }

    @Test
    public void partialWordFails() throws Exception {
        queryResult = topicGraph.getAtomsByTitleQuery("Arth", filter);
        assertEquals(0, queryResult.size());
    }

    @Test
    public void matchingWildcardSucceeds() throws Exception {
        queryResult = topicGraph.getAtomsByTitleQuery("Arth*", filter);
        assertEquals(1, queryResult.size());
    }

    @Test
    public void nonMatchingWildcardFails() throws Exception {
        queryResult = topicGraph.getAtomsByTitleQuery("Zaph*", filter);
        assertEquals(0, queryResult.size());
    }

    @Test
    public void caseInsensitiveMatchSucceeds() throws Exception {
        queryResult = topicGraph.getAtomsByTitleQuery("ARTHUR Dent", filter);
        assertEquals(1, queryResult.size());
        queryResult = topicGraph.getAtomsByTitleQuery("aRTHur", filter);
        assertEquals(1, queryResult.size());
        queryResult = topicGraph.getAtomsByTitleQuery("*dENT", filter);
        assertEquals(1, queryResult.size());
    }

    @Test
    public void quotedExactMatchSucceeds() throws Exception {
        queryResult = topicGraph.getAtomsByTitleQuery("\"Arthur Dent\"", filter);
        assertEquals(1, queryResult.size());
        assertEquals(arthur.getId(), queryResult.iterator().next().getId());
    }
}
