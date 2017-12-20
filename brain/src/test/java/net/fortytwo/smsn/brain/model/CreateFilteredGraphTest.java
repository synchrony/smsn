package net.fortytwo.smsn.brain.model;

import net.fortytwo.smsn.brain.BrainTestBase;
import net.fortytwo.smsn.brain.model.entities.Note;
import org.junit.Test;

import java.io.IOException;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertNull;
import static org.junit.Assert.assertTrue;

public class CreateFilteredGraphTest extends BrainTestBase {

    @Override
    protected TopicGraph createTopicGraph() throws IOException {
        return createTinkerTopicGraph();
        //return createNeo4jTopicGraph();
    }

    @Test
    public void testFilteredCopy() throws Exception {
        assertEquals(0, countNotes(topicGraph));

        // unfiltered
        Note root = importNoteFromFile("io/wiki/wiki-example-4.txt");
        root.setLabel("William James");
        assertEquals(DefaultSources.PERSONAL, root.getSource());
        assertEquals(23, countNotes(topicGraph));
        assertEquals(7, childList(root).size());
        assertEquals("some works by William James", childList(root).get(0).getLabel());
        assertEquals("William James's depression", childList(root).get(3).getLabel());
        assertEquals(DefaultSources.PUBLIC, childList(root).get(0).getSource());
        assertEquals(DefaultSources.PRIVATE, childList(root).get(3).getSource());
        assertEquals(3, childList(childList(root).get(0)).size());
        assertEquals(2, childList(childList(root).get(3)).size());

        // filtered
        Filter publicFilter = new Filter(0f, 0.5f, DefaultSources.PERSONAL, DefaultSources.PERSONAL);
        assertTrue(publicFilter.test(root));
        TopicGraph filteredGraph = topicGraph.createFilteredGraph(publicFilter);
        assertEquals(22, countNotes(filteredGraph));
        root = getNote(filteredGraph.getTopicById(root.getTopic().getId()).get());
        assertEquals(DefaultSources.PERSONAL, root.getSource());
        assertEquals("William James", root.getLabel());

        assertEquals(7, childList(root).size());
        assertEquals("some works by William James", childList(root).get(0).getLabel());
        assertEquals("", childList(root).get(3).getLabel());
        assertEquals(DefaultSources.PUBLIC, childList(root).get(0).getSource());
        assertEquals(DefaultSources.PRIVATE, childList(root).get(3).getSource());
        assertEquals(3, childList(childList(root).get(0)).size());
        assertNull(childList(root).get(3).getFirst());
    }
}
