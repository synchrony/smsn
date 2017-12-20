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
        assertTrue(filter.test(createNote(DefaultSources.UNIVERSAL, 0.5f)));
        assertFalse(filter.test(createNote(DefaultSources.UNIVERSAL, 0.4f)));
    }

    @Test
    public void missingNonessentialPropertiesAreTolerated() {
        Filter filter = Filter.noFilter();

        Note note = createNote("public", 0.75f);
        note.setLabel(null);
        assertTrue(filter.test(note));
    }

    @Test
    public void missingSourceFailsTest() throws Exception {
        Filter filter = Filter.noFilter();
        assertFalse(filter.test(createNote(null, 0.75f)));
    }

    @Test
    public void missingWeightFailsTest() throws Exception {
        Filter filter = Filter.noFilter();
        assertFalse(filter.test(createNote(DefaultSources.UNIVERSAL, (Float) null)));
    }

    @Test
    public void noFilterIncludesAllSources() throws Exception {
        Filter filter = Filter.noFilter();
        assertTrue(filter.test(createNote(DefaultSources.UNIVERSAL, 1.0f)));
        assertTrue(filter.test(createNote(DefaultSources.PRIVATE, 1.0f)));
        assertTrue(filter.test(createNote(DefaultSources.PERSONAL, 1.0f)));
        assertTrue(filter.test(createNote(DefaultSources.PUBLIC, 1.0f)));
        assertTrue(filter.test(createNote(DefaultSources.UNIVERSAL, 1.0f)));
    }

    @Test
    public void noFilterIncludesAllWeight() throws Exception {
        Filter filter = Filter.noFilter();
        assertTrue(filter.test(createNote(DefaultSources.UNIVERSAL, 0.0f)));
        assertTrue(filter.test(createNote(DefaultSources.UNIVERSAL, 0.25f)));
        assertTrue(filter.test(createNote(DefaultSources.UNIVERSAL, 0.5f)));
        assertTrue(filter.test(createNote(DefaultSources.UNIVERSAL, 0.75f)));
        assertTrue(filter.test(createNote(DefaultSources.UNIVERSAL, 1.0f)));
    }

    private Note createNote(final String source, final Float weight) {
        Note note = new NoteDTO();
        note.setSource(source);
        note.setWeight(weight);
        return note;
    }

    @Override
    protected TopicGraph createTopicGraph() throws IOException {
        return createTinkerTopicGraph();
    }
}
