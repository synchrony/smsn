package net.fortytwo.smsn.brain.model;

import net.fortytwo.smsn.brain.model.entities.Atom;
import org.junit.Test;

import static org.junit.Assert.assertFalse;
import static org.junit.Assert.assertTrue;

public class FilterTest {
    @Test
    public void minimumSharabilityIsInclusive() throws Exception {
        Filter filter = new Filter(0.5f, 0.5f, 0.5f, 0.5f);
        assertTrue(filter.test(createAtom(0.5f, 1.0f)));
        assertFalse(filter.test(createAtom(0.4f, 1.0f)));
    }

    @Test
    public void minimumWeightIsInclusive() throws Exception {
        Filter filter = new Filter(0.5f, 0.5f, 0.5f, 0.5f);
        assertTrue(filter.test(createAtom(1.0f, 0.5f)));
        assertFalse(filter.test(createAtom(1.0f, 0.4f)));
    }

    @Test
    public void missingSharabilityFailsTest() throws Exception {
        Filter filter = Filter.noFilter();
        assertFalse(filter.test(createAtom(null, 1.0f)));
    }

    @Test
    public void missingWeightFailsTest() throws Exception {
        Filter filter = Filter.noFilter();
        assertFalse(filter.test(createAtom(1.0f, null)));
    }

    @Test
    public void noFilterIncludesAllSharability() throws Exception {
        Filter filter = Filter.noFilter();
        assertTrue(filter.test(createAtom(0.0f, 1.0f)));
        assertTrue(filter.test(createAtom(0.25f, 1.0f)));
        assertTrue(filter.test(createAtom(0.5f, 1.0f)));
        assertTrue(filter.test(createAtom(0.75f, 1.0f)));
        assertTrue(filter.test(createAtom(1.0f, 1.0f)));
    }

    @Test
    public void noFilterIncludesAllWeight() throws Exception {
        Filter filter = Filter.noFilter();
        assertTrue(filter.test(createAtom(1.0f, 0.0f)));
        assertTrue(filter.test(createAtom(1.0f, 0.25f)));
        assertTrue(filter.test(createAtom(1.0f, 0.5f)));
        assertTrue(filter.test(createAtom(1.0f, 0.75f)));
        assertTrue(filter.test(createAtom(1.0f, 1.0f)));
    }

    private Atom createAtom(final Float sharability, final Float weight) {
        Atom atom = new AtomBase();
        atom.setSharability(sharability);
        atom.setWeight(weight);
        return atom;
    }
}
