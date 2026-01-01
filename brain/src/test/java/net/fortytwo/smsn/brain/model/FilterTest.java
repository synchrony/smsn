package net.fortytwo.smsn.brain.model;

import net.fortytwo.smsn.brain.BrainTestBase;
import net.fortytwo.smsn.brain.model.dto.NoteDTO;
import net.fortytwo.smsn.brain.model.entities.Note;
import org.junit.Test;

import java.io.IOException;
import java.util.Collections;
import java.util.HashSet;
import java.util.Set;

import static org.junit.Assert.assertFalse;
import static org.junit.Assert.assertTrue;

public class FilterTest extends BrainTestBase {

    @Test
    public void minimumWeightIsInclusive() throws Exception {
        Set<String> sources = new HashSet<>();
        sources.add(DefaultSources.PERSONAL);
        sources.add(DefaultSources.PUBLIC);
        sources.add(DefaultSources.UNIVERSAL);
        Filter filter = new Filter(0.5f, 0.5f, sources, DefaultSources.PERSONAL);
        assertTrue(filter.test(createNote(DefaultSources.UNIVERSAL, 0.5f)));
        assertFalse(filter.test(createNote(DefaultSources.UNIVERSAL, 0.4f)));
    }

    @Test
    public void missingNonessentialPropertiesAreTolerated() {
        Filter filter = Filter.noFilter();

        Note note = createNote("public", 0.75f);
        Note.setTitle(note, null);
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

    @Test
    public void emptyIncludedSourcesIncludesAll() throws Exception {
        Filter filter = new Filter(0f, 0.5f, Collections.emptySet(), DefaultSources.PRIVATE);
        assertTrue(filter.test(createNote(DefaultSources.PRIVATE, 0.5f)));
        assertTrue(filter.test(createNote(DefaultSources.PERSONAL, 0.5f)));
        assertTrue(filter.test(createNote(DefaultSources.PUBLIC, 0.5f)));
        assertTrue(filter.test(createNote(DefaultSources.UNIVERSAL, 0.5f)));
    }

    @Test
    public void includedSourcesFilterCorrectly() throws Exception {
        Set<String> sources = new HashSet<>();
        sources.add(DefaultSources.PUBLIC);
        sources.add(DefaultSources.UNIVERSAL);
        Filter filter = new Filter(0f, 0.5f, sources, DefaultSources.PUBLIC);

        assertFalse(filter.test(createNote(DefaultSources.PRIVATE, 0.5f)));
        assertFalse(filter.test(createNote(DefaultSources.PERSONAL, 0.5f)));
        assertTrue(filter.test(createNote(DefaultSources.PUBLIC, 0.5f)));
        assertTrue(filter.test(createNote(DefaultSources.UNIVERSAL, 0.5f)));
    }

    @Test
    public void singleIncludedSourceWorks() throws Exception {
        Set<String> sources = Collections.singleton(DefaultSources.PERSONAL);
        Filter filter = new Filter(0f, 0.5f, sources, DefaultSources.PERSONAL);

        assertFalse(filter.test(createNote(DefaultSources.PRIVATE, 0.5f)));
        assertTrue(filter.test(createNote(DefaultSources.PERSONAL, 0.5f)));
        assertFalse(filter.test(createNote(DefaultSources.PUBLIC, 0.5f)));
        assertFalse(filter.test(createNote(DefaultSources.UNIVERSAL, 0.5f)));
    }

    @Test
    public void deprecatedMinSourceConstructorStillWorks() throws Exception {
        // The deprecated constructor should still work for backwards compatibility
        Filter filter = new Filter(0.5f, 0.5f, DefaultSources.PERSONAL, DefaultSources.PERSONAL);
        // Should include PERSONAL, PUBLIC, UNIVERSAL (indices >= PERSONAL)
        assertFalse(filter.test(createNote(DefaultSources.PRIVATE, 0.5f)));
        assertTrue(filter.test(createNote(DefaultSources.PERSONAL, 0.5f)));
        assertTrue(filter.test(createNote(DefaultSources.PUBLIC, 0.5f)));
        assertTrue(filter.test(createNote(DefaultSources.UNIVERSAL, 0.5f)));
    }

    private Note createNote(final String source, final Float weight) {
        Note note = NoteDTO.createNew();
        Note.setSource(note, source);
        Note.setWeight(note, weight);
        return note;
    }

    @Override
    protected TopicGraph createTopicGraph() throws IOException {
        return createTinkerTopicGraph();
    }
}
