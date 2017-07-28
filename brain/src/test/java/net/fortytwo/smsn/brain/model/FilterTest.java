package net.fortytwo.smsn.brain.model;

import net.fortytwo.smsn.brain.BrainTestBase;
import net.fortytwo.smsn.brain.model.dto.NoteDTO;
import net.fortytwo.smsn.brain.model.entities.Note;
import org.junit.Test;

import java.io.IOException;

import static org.junit.Assert.assertFalse;
import static org.junit.Assert.assertTrue;

public class FilterTest extends BrainTestBase {

    @Test
    public void minimumWeightIsInclusive() throws Exception {
        Filter filter = new Filter(0.5f, 0.5f, DefaultSources.PERSONAL, DefaultSources.PERSONAL);
        assertTrue(filter.test(createAtom(DefaultSources.UNIVERSAL, 0.5f)));
        assertFalse(filter.test(createAtom(DefaultSources.UNIVERSAL, 0.4f)));
    }

    @Test
    public void missingNonessentialPropertiesAreTolerated() {
        Filter filter = Filter.noFilter();

        Note atom = createAtom("public", 0.75f);
        atom.setTitle(null);
        assertTrue(filter.test(atom));
    }

    @Test
    public void missingSourceFailsTest() throws Exception {
        Filter filter = Filter.noFilter();
        assertFalse(filter.test(createAtom(null, 0.75f)));
    }

    @Test
    public void missingWeightFailsTest() throws Exception {
        Filter filter = Filter.noFilter();
        assertFalse(filter.test(createAtom(DefaultSources.UNIVERSAL, (Float) null)));
    }

    @Test
    public void noFilterIncludesAllSources() throws Exception {
        Filter filter = Filter.noFilter();
        assertTrue(filter.test(createAtom(DefaultSources.UNIVERSAL, 1.0f)));
        assertTrue(filter.test(createAtom(DefaultSources.PRIVATE, 1.0f)));
        assertTrue(filter.test(createAtom(DefaultSources.PERSONAL, 1.0f)));
        assertTrue(filter.test(createAtom(DefaultSources.PUBLIC, 1.0f)));
        assertTrue(filter.test(createAtom(DefaultSources.UNIVERSAL, 1.0f)));
    }

    @Test
    public void noFilterIncludesAllWeight() throws Exception {
        Filter filter = Filter.noFilter();
        assertTrue(filter.test(createAtom(DefaultSources.UNIVERSAL, 0.0f)));
        assertTrue(filter.test(createAtom(DefaultSources.UNIVERSAL, 0.25f)));
        assertTrue(filter.test(createAtom(DefaultSources.UNIVERSAL, 0.5f)));
        assertTrue(filter.test(createAtom(DefaultSources.UNIVERSAL, 0.75f)));
        assertTrue(filter.test(createAtom(DefaultSources.UNIVERSAL, 1.0f)));
    }

    private Note createAtom(final String source, final Float weight) {
        Note atom = new NoteDTO();
        atom.setSource(source);
        atom.setWeight(weight);
        return atom;
    }

    @Override
    protected TopicGraph createTopicGraph() throws IOException {
        return createTinkerTopicGraph();
    }
}
