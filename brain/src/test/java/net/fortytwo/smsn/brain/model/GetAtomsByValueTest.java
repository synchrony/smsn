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
    protected TopicGraph createAtomGraph() throws IOException {
        return createNeo4jAtomGraph();
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
        result = topicGraph.getAtomsByTitleQuery("Zaphod", filter);
        assertEquals(0, result.size());
    }

    @Test
    public void completelyMatchingValueSucceeds() throws Exception {
        result = topicGraph.getAtomsByTitleQuery("Arthur Dent", filter);
        assertEquals(1, result.size());
    }

    @Test
    public void subsetOfWordsMatchingSucceeds() throws Exception {
        result = topicGraph.getAtomsByTitleQuery("Arthur Beeblebrox", filter);
        assertEquals(1, result.size());
    }

    @Test
    public void outOfOrderWordsSucceeds() throws Exception {
        result = topicGraph.getAtomsByTitleQuery("Dent Arthur", filter);
        assertEquals(1, result.size());
    }

    @Test
    public void partialValueSucceeds() throws Exception {
        result = topicGraph.getAtomsByTitleQuery("Arthur", filter);
        assertEquals(1, result.size());
    }

    @Test
    public void partialWordFails() throws Exception {
        result = topicGraph.getAtomsByTitleQuery("Arth", filter);
        assertEquals(0, result.size());
    }

    @Test
    public void matchingWildcardSucceeds() throws Exception {
        result = topicGraph.getAtomsByTitleQuery("Arth*", filter);
        assertEquals(1, result.size());
    }

    @Test
    public void nonMatchingWildcardFails() throws Exception {
        result = topicGraph.getAtomsByTitleQuery("Zaph*", filter);
        assertEquals(0, result.size());
    }

    @Test
    public void caseInsensitiveMatchSucceeds() throws Exception {
        result = topicGraph.getAtomsByTitleQuery("ARTHUR Dent", filter);
        assertEquals(1, result.size());
        result = topicGraph.getAtomsByTitleQuery("aRTHur", filter);
        assertEquals(1, result.size());
        result = topicGraph.getAtomsByTitleQuery("*dENT", filter);
        assertEquals(1, result.size());
    }

    @Test
    public void quotedExactMatchSucceeds() throws Exception {
        result = topicGraph.getAtomsByTitleQuery("\"Arthur Dent\"", filter);
        assertEquals(1, result.size());
        assertEquals(arthur.getId(), result.iterator().next().getId());
    }
}
