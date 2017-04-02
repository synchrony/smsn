package net.fortytwo.smsn.brain.model;

import net.fortytwo.smsn.brain.BrainTestBase;
import net.fortytwo.smsn.brain.model.entities.Atom;
import org.junit.Test;

import java.io.IOException;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertNull;
import static org.junit.Assert.assertTrue;

public class CreateFilteredGraphTest extends BrainTestBase {

    @Override
    protected TopicGraph createAtomGraph() throws IOException {
        return createTinkerAtomGraph();
        //return createNeo4jAtomGraph();
    }

    @Test
    public void testFilteredCopy() throws Exception {
        assertEquals(0, countAtoms(topicGraph));

        // unfiltered
        Atom root = importAtomFromFile("io/wiki/wiki-example-4.txt");
        root.setTitle("William James");
        assertEquals(0.5, root.getSharability(), 0);
        assertEquals(23, countAtoms(topicGraph));
        assertEquals(7, childList(root).size());
        assertEquals("some works by William James", childList(root).get(0).getTitle());
        assertEquals("William James's depression", childList(root).get(3).getTitle());
        assertEquals(0.75, childList(root).get(0).getSharability(), 0);
        assertEquals(0.25, childList(root).get(3).getSharability(), 0);
        assertEquals(3, childList(childList(root).get(0)).size());
        assertEquals(2, childList(childList(root).get(3)).size());

        // filtered
        Filter publicFilter = new Filter(0f, 0.5f, 0.5f, 0.5f);
        assertTrue(publicFilter.test(root));
        TopicGraph filteredGraph = topicGraph.createFilteredGraph(publicFilter);
        assertEquals(22, countAtoms(filteredGraph));
        root = filteredGraph.getAtomById(root.getId()).get();
        assertEquals(0.5, root.getSharability(), 0);
        assertEquals("William James", root.getTitle());

        assertEquals(7, childList(root).size());
        assertEquals("some works by William James", childList(root).get(0).getTitle());
        assertEquals("", childList(root).get(3).getTitle());
        assertEquals(0.75, childList(root).get(0).getSharability(), 0);
        assertEquals(0.25, childList(root).get(3).getSharability(), 0);
        assertEquals(3, childList(childList(root).get(0)).size());
        assertNull(childList(root).get(3).getChildren());
    }
}
