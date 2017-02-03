package net.fortytwo.smsn.brain.model;

import net.fortytwo.smsn.brain.BrainTestBase;
import org.junit.Test;

import java.io.IOException;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertNotNull;

public class GetAtomsByIdTest extends BrainTestBase {

    @Override
    protected AtomGraph createAtomGraph() throws IOException {
        return createTinkerAtomGraph();
    }

    @Test
    public void allLegalIdsMayBeUsed() throws Exception {
        String[] ids = new String[]{"aaaaaaa", "0000000", "a1b2c3d", "-------", "_______", "-_0-ad_"};

        int count = 0;
        for (String id : ids) {
            Atom atom = atomGraph.createAtomWithProperties(filter, id);
            atom.setTitle("atom #" + ++count);
        }

        for (int i = 0; i < ids.length; i++) {
            Atom atom = atomGraph.getAtomById(ids[i]);
            assertNotNull(atom);
            assertEquals("atom #" + (i+1), atom.getTitle());
        }
    }

    @Test
    public void testGetAtoms() throws Exception {
        Atom chaos = atomGraph.createAtomWithProperties(filter, null);
        chaos.setTitle("Chaos");
        Atom tartarus = atomGraph.createAtomWithProperties(filter, null);
        tartarus.setTitle("Tartarus");
        Atom gaia = atomGraph.createAtomWithProperties(filter, null);
        gaia.setTitle("Gaia");
        Atom eros = atomGraph.createAtomWithProperties(filter, null);
        eros.setTitle("Eros");
        Atom nyx = atomGraph.createAtomWithProperties(filter, null);
        nyx.setTitle("Nyx");
        Atom erebus = atomGraph.createAtomWithProperties(filter, null);
        erebus.setTitle("Erebus");
        AtomList children = atomGraph.createAtomList(tartarus, gaia, eros, nyx, erebus);
        chaos.setNotes(children);

        //System.out.println(chaos.getId());
        //System.out.println(chaos.getValue());

        // getAtoms returns a list of only atom vertices, excluding list vertices
        int count = 0;
        Iterable<Atom> atoms = atomGraph.getAllAtoms();
        for (Atom a : atoms) {
            count++;
            //System.out.println(a.getId());
            //System.out.println(a.getValue());
        }
        assertEquals(6, count);
    }
}
