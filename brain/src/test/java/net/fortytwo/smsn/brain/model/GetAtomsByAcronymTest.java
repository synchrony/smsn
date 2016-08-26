package net.fortytwo.smsn.brain.model;

import net.fortytwo.smsn.SemanticSynchrony;
import net.fortytwo.smsn.brain.model.pg.PGAtom;
import org.junit.Test;

import java.util.Collection;

import static org.junit.Assert.assertEquals;

public class GetAtomsByAcronymTest extends AtomGraphTest {

    @Test
    public void testAcronymSearch() throws Exception {
        Atom a = atomGraph.createAtom(filter, null);
        a.setValue("Arthur\tP.  Dent ");
        Atom t = atomGraph.createAtom(filter, null);
        t.setValue("Arthur's moth-eaten towel");
        Atom l = atomGraph.createAtom(filter, null);
        l.setValue("ooooooooo0ooooooooo1ooooooooo2ooooooooo3ooooooooo4ooooooooo5ooooooooo6ooooooooo7" +
                "ooooooooo8ooooooooo9oooooooooAoooooooooBoooooooooCoooooooooDoooooooooEoooooooooF");
        atomGraph.reindexAtom(a);
        atomGraph.reindexAtom(t);
        atomGraph.reindexAtom(l);

        Collection<Atom> result;

        // oops. This is not a full-text query.
        result = atomGraph.getAtomsByAcronym("Arthur*", filter);
        assertEquals(0, result.size());

        // l has not been indexed because its value is too long
        result = atomGraph.getAtomsByAcronym("o", filter);
        assertEquals(0, result.size());

        for (Atom atom : atomGraph.getAllAtoms()) {
            System.out.println(atom.getId() + ": "
                    + ((PGAtom) atom).asVertex().property(SemanticSynchrony.ACRONYM));
        }

        // exact acronym match
        // capitalization, punctuation, and idiosyncrasies of white space are ignored
        result = atomGraph.getAtomsByAcronym("apd", filter);
        assertEquals(1, result.size());
        assertEquals(a.getId(), result.iterator().next().getId());

        // hyphens and underscores are treated as white space, while apostrophes and other punctuation are ignored
        result = atomGraph.getAtomsByAcronym("amet", filter);
        assertEquals(1, result.size());
        assertEquals(t.getId(), result.iterator().next().getId());

        // acronym search is case insensitive
        result = atomGraph.getAtomsByAcronym("APD", filter);
        assertEquals(1, result.size());
        assertEquals(a.getId(), result.iterator().next().getId());
    }
}
