package net.fortytwo.smsn.brain.model;

import org.junit.Before;
import org.junit.Test;

import static org.junit.Assert.assertEquals;

public class GetAtomsByValueTest extends AtomGraphTest {
    private Atom arthur;

    @Before
    @Override
    public void setUp() throws Exception {
        super.setUp();

        arthur = atomGraph.createAtom(filter, null);
        arthur.setValue("Arthur Dent");
        atomGraph.reindexAtom(arthur);
    }

    @Test
    public void nonMatchingValueFails() throws Exception {
        result = atomGraph.getAtomsByValue("Zaphod", filter);
        assertEquals(0, result.size());
    }

    @Test
    public void completelyMatchingValueSucceeds() throws Exception {
        result = atomGraph.getAtomsByValue("Arthur Dent", filter);
        assertEquals(1, result.size());
    }

    @Test
    public void partialValueSucceeds() throws Exception {
        result = atomGraph.getAtomsByValue("Arthur", filter);
        assertEquals(1, result.size());
    }

    @Test
    public void partialWordFails() throws Exception {
        result = atomGraph.getAtomsByValue("Arth", filter);
        assertEquals(0, result.size());
    }

    @Test
    public void matchingWildcardSucceeds() throws Exception {
        result = atomGraph.getAtomsByValue("Arth*", filter);
        assertEquals(1, result.size());
    }

    @Test
    public void nonMatchingWildcardFails() throws Exception {
        result = atomGraph.getAtomsByValue("Zaph*", filter);
        assertEquals(0, result.size());
    }

    @Test
    public void caseInsensitiveMatchSucceeds() throws Exception {
        result = atomGraph.getAtomsByValue("ARTHUR Dent", filter);
        assertEquals(1, result.size());
        result = atomGraph.getAtomsByValue("aRTHur", filter);
        assertEquals(1, result.size());
        result = atomGraph.getAtomsByValue("*dENT", filter);
        assertEquals(1, result.size());
    }

    @Test
    public void quotedExactMatchSucceeds() throws Exception {
        result = atomGraph.getAtomsByValue("\"Arthur Dent\"", filter);
        assertEquals(1, result.size());
        assertEquals(arthur.getId(), result.iterator().next().getId());
    }
}
