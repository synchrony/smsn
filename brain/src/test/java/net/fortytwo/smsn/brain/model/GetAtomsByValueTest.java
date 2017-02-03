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
        result = atomGraph.getAtomsByValueQuery("Zaphod", filter);
        assertEquals(0, result.size());
    }

    @Test
    public void completelyMatchingValueSucceeds() throws Exception {
        result = atomGraph.getAtomsByValueQuery("Arthur Dent", filter);
        assertEquals(1, result.size());
    }

    @Test
    public void subsetOfWordsMatchingSucceeds() throws Exception {
        result = atomGraph.getAtomsByValueQuery("Arthur Beeblebrox", filter);
        assertEquals(1, result.size());
    }

    @Test
    public void outOfOrderWordsSucceeds() throws Exception {
        result = atomGraph.getAtomsByValueQuery("Dent Arthur", filter);
        assertEquals(1, result.size());
    }

    @Test
    public void partialValueSucceeds() throws Exception {
        result = atomGraph.getAtomsByValueQuery("Arthur", filter);
        assertEquals(1, result.size());
    }

    @Test
    public void partialWordFails() throws Exception {
        result = atomGraph.getAtomsByValueQuery("Arth", filter);
        assertEquals(0, result.size());
    }

    @Test
    public void matchingWildcardSucceeds() throws Exception {
        result = atomGraph.getAtomsByValueQuery("Arth*", filter);
        assertEquals(1, result.size());
    }

    @Test
    public void nonMatchingWildcardFails() throws Exception {
        result = atomGraph.getAtomsByValueQuery("Zaph*", filter);
        assertEquals(0, result.size());
    }

    @Test
    public void caseInsensitiveMatchSucceeds() throws Exception {
        result = atomGraph.getAtomsByValueQuery("ARTHUR Dent", filter);
        assertEquals(1, result.size());
        result = atomGraph.getAtomsByValueQuery("aRTHur", filter);
        assertEquals(1, result.size());
        result = atomGraph.getAtomsByValueQuery("*dENT", filter);
        assertEquals(1, result.size());
    }

    @Test
    public void quotedExactMatchSucceeds() throws Exception {
        result = atomGraph.getAtomsByValueQuery("\"Arthur Dent\"", filter);
        assertEquals(1, result.size());
        assertEquals(arthur.getId(), result.iterator().next().getId());
    }
}
