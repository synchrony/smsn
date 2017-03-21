package net.fortytwo.smsn.brain.model;

import net.fortytwo.smsn.brain.BrainTestBase;
import net.fortytwo.smsn.brain.model.entities.Atom;
import org.junit.Test;

import java.io.IOException;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertNotNull;
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
        assertEquals(7, root.getNotes().toJavaList().size());
        assertEquals("some works by William James", root.getNotes().toJavaList().get(0).getTitle());
        assertEquals("William James's depression", root.getNotes().toJavaList().get(3).getTitle());
        assertEquals(0.75, root.getNotes().toJavaList().get(0).getSharability(), 0);
        assertEquals(0.25, root.getNotes().toJavaList().get(3).getSharability(), 0);
        assertEquals(3, root.getNotes().toJavaList().get(0).getNotes().toJavaList().size());
        assertEquals(2, root.getNotes().toJavaList().get(3).getNotes().toJavaList().size());

        // filtered
        Filter publicFilter = new Filter(0f, 0.5f, 0.5f, 0.5f);
        assertTrue(publicFilter.isVisible(root));
        TopicGraph filteredGraph = topicGraph.createFilteredGraph(publicFilter);
        assertEquals(22, countAtoms(filteredGraph));
        root = filteredGraph.getAtomById(root.getId());
        assertNotNull(root);
        assertEquals(0.5, root.getSharability(), 0);
        assertEquals("William James", root.getTitle());

        assertEquals(7, root.getNotes().toJavaList().size());
        assertEquals("some works by William James", root.getNotes().toJavaList().get(0).getTitle());
        assertEquals("", root.getNotes().toJavaList().get(3).getTitle());
        assertEquals(0.75, root.getNotes().toJavaList().get(0).getSharability(), 0);
        assertEquals(0.25, root.getNotes().toJavaList().get(3).getSharability(), 0);
        assertEquals(3, root.getNotes().toJavaList().get(0).getNotes().toJavaList().size());
        assertNull(root.getNotes().toJavaList().get(3).getNotes());
    }
}
