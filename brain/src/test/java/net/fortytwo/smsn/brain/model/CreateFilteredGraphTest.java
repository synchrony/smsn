package net.fortytwo.smsn.brain.model;

import net.fortytwo.smsn.brain.BrainTestBase;
import org.junit.Test;

import java.io.IOException;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertNotNull;
import static org.junit.Assert.assertNull;
import static org.junit.Assert.assertTrue;

public class CreateFilteredGraphTest extends BrainTestBase {

    @Override
    protected AtomGraph createAtomGraph() throws IOException {
        return createTinkerAtomGraph();
        //return createNeo4jAtomGraph();
    }

    @Test
    public void testFilteredCopy() throws Exception {
        assertEquals(0, countAtoms(atomGraph));

        // unfiltered
        Atom root = importAtomFromFile("io/wiki/wiki-example-4.txt");
        root.setValue("William James");
        assertEquals(0.5, root.getSharability(), 0);
        assertEquals(23, countAtoms(atomGraph));
        assertEquals(7, root.getNotes().toJavaList().size());
        assertEquals("some works by William James", root.getNotes().toJavaList().get(0).getValue());
        assertEquals("William James's depression", root.getNotes().toJavaList().get(3).getValue());
        assertEquals(0.75, root.getNotes().toJavaList().get(0).getSharability(), 0);
        assertEquals(0.25, root.getNotes().toJavaList().get(3).getSharability(), 0);
        assertEquals(3, root.getNotes().toJavaList().get(0).getNotes().toJavaList().size());
        assertEquals(2, root.getNotes().toJavaList().get(3).getNotes().toJavaList().size());

        // filtered
        Filter publicFilter = new Filter(0f, 1f, 0.5f, 0.25f, 1f, 0.5f);
        assertTrue(publicFilter.isVisible(root));
        AtomGraph filteredGraph = atomGraph.createFilteredGraph(publicFilter);
        assertEquals(22, countAtoms(filteredGraph));
        root = filteredGraph.getAtomById(root.getId());
        assertNotNull(root);
        assertEquals(0.5, root.getSharability(), 0);
        assertEquals("William James", root.getValue());

        assertEquals(7, root.getNotes().toJavaList().size());
        assertEquals("some works by William James", root.getNotes().toJavaList().get(0).getValue());
        assertEquals("", root.getNotes().toJavaList().get(3).getValue());
        assertEquals(0.75, root.getNotes().toJavaList().get(0).getSharability(), 0);
        assertEquals(0.25, root.getNotes().toJavaList().get(3).getSharability(), 0);
        assertEquals(3, root.getNotes().toJavaList().get(0).getNotes().toJavaList().size());
        assertNull(root.getNotes().toJavaList().get(3).getNotes());
    }
}
