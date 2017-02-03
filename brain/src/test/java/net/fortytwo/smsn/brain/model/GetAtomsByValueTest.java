package net.fortytwo.smsn.brain.model;

import net.fortytwo.smsn.brain.BrainTestBase;
import org.junit.Before;
import org.junit.Test;

import java.io.IOException;

import static org.junit.Assert.assertEquals;

public class GetAtomsByValueTest extends BrainTestBase {
    private Atom arthur;

    @Override
    protected AtomGraph createAtomGraph() throws IOException {
        return createNeo4jAtomGraph();
    }

    @Before
    @Override
    public void setUp() throws Exception {
        super.setUp();

        arthur = atomGraph.createAtomWithProperties(filter, null);
        arthur.setTitle("Arthur Dent");
        atomGraph.reindexAtom(arthur);
    }

    @Test
    public void nonMatchingValueFails() throws Exception {
        result = atomGraph.getAtomsByTitleQuery("Zaphod", filter);
        assertEquals(0, result.size());
    }

    @Test
    public void completelyMatchingValueSucceeds() throws Exception {
        result = atomGraph.getAtomsByTitleQuery("Arthur Dent", filter);
        assertEquals(1, result.size());
    }

    @Test
    public void subsetOfWordsMatchingSucceeds() throws Exception {
        result = atomGraph.getAtomsByTitleQuery("Arthur Beeblebrox", filter);
        assertEquals(1, result.size());
    }

    @Test
    public void outOfOrderWordsSucceeds() throws Exception {
        result = atomGraph.getAtomsByTitleQuery("Dent Arthur", filter);
        assertEquals(1, result.size());
    }

    @Test
    public void partialValueSucceeds() throws Exception {
        result = atomGraph.getAtomsByTitleQuery("Arthur", filter);
        assertEquals(1, result.size());
    }

    @Test
    public void partialWordFails() throws Exception {
        result = atomGraph.getAtomsByTitleQuery("Arth", filter);
        assertEquals(0, result.size());
    }

    @Test
    public void matchingWildcardSucceeds() throws Exception {
        result = atomGraph.getAtomsByTitleQuery("Arth*", filter);
        assertEquals(1, result.size());
    }

    @Test
    public void nonMatchingWildcardFails() throws Exception {
        result = atomGraph.getAtomsByTitleQuery("Zaph*", filter);
        assertEquals(0, result.size());
    }

    @Test
    public void caseInsensitiveMatchSucceeds() throws Exception {
        result = atomGraph.getAtomsByTitleQuery("ARTHUR Dent", filter);
        assertEquals(1, result.size());
        result = atomGraph.getAtomsByTitleQuery("aRTHur", filter);
        assertEquals(1, result.size());
        result = atomGraph.getAtomsByTitleQuery("*dENT", filter);
        assertEquals(1, result.size());
    }

    @Test
    public void quotedExactMatchSucceeds() throws Exception {
        result = atomGraph.getAtomsByTitleQuery("\"Arthur Dent\"", filter);
        assertEquals(1, result.size());
        assertEquals(arthur.getId(), result.iterator().next().getId());
    }
}
