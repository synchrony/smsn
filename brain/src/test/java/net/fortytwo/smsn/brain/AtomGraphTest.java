package net.fortytwo.smsn.brain;

import com.tinkerpop.blueprints.impls.neo4j.Neo4jGraph;
import net.fortytwo.smsn.brain.model.Atom;
import net.fortytwo.smsn.brain.model.AtomGraph;
import net.fortytwo.smsn.brain.model.AtomList;
import net.fortytwo.smsn.brain.model.pg.PGAtomGraph;
import org.junit.After;
import org.junit.Before;
import org.junit.Test;

import java.io.File;
import java.util.Collection;

import static org.junit.Assert.assertEquals;

/**
 * @author Joshua Shinavier (http://fortytwo.net)
 */
public class AtomGraphTest {
    private Neo4jGraph graph;
    private AtomGraph atomGraph;

    @Before
    public void setUp() throws Exception {
        File dir = File.createTempFile("smsn", "test");
        dir.delete();
        dir.mkdir();
        dir.deleteOnExit();

        graph = new Neo4jGraph(dir.getPath());

        atomGraph = new PGAtomGraph(graph);
    }

    @After
    public void tearDown() throws Exception {
        graph.shutdown();
    }

    @Test
    public void testGetAtoms() throws Exception {
        Filter f = new Filter();

        Atom chaos = atomGraph.createAtom(f, null);
        chaos.setValue("Chaos");
        Atom tartarus = atomGraph.createAtom(f, null);
        tartarus.setValue("Tartarus");
        Atom gaia = atomGraph.createAtom(f, null);
        gaia.setValue("Gaia");
        Atom eros = atomGraph.createAtom(f, null);
        eros.setValue("Eros");
        Atom nyx = atomGraph.createAtom(f, null);
        nyx.setValue("Nyx");
        Atom erebus = atomGraph.createAtom(f, null);
        erebus.setValue("Erebus");
        AtomList children = atomGraph.createAtomList(tartarus, gaia, eros, nyx, erebus);
        chaos.setNotes(children);

        // getAtoms returns a list of only atom vertices, excluding list vertices
        int count = 0;
        Iterable<Atom> atoms = atomGraph.getAllAtoms();
        for (Atom a : atoms) {
            count++;
            System.out.println(a.getValue());
        }
        assertEquals(6, count);
    }

    @Test
    public void testFulltextSearch() throws Exception {
        Filter f = new Filter();

        Atom a = atomGraph.createAtom(f, null);
        a.setValue("Arthur Dent");
        atomGraph.addAtomToIndices(a);
        //a.setValue("Arthur");

        Collection<Atom> result;

        // Partial searches don't work
        result = atomGraph.getAtomsByFulltextQuery("Arthur", f);
        assertEquals(0, result.size());

        // Currently no ability to search on multiple keywords
        result = atomGraph.getAtomsByFulltextQuery("Arthur Dent", f);
        assertEquals(0, result.size());

        // Wildcards are supported
        result = atomGraph.getAtomsByFulltextQuery("Arthur*", f);
        assertEquals(1, result.size());
        assertEquals(a.asVertex().getId(), result.iterator().next().asVertex().getId());

        // Search is case-insensitive
        result = atomGraph.getAtomsByFulltextQuery("arthur*", f);
        assertEquals(1, result.size());

        // Exact matches in quotes
        result = atomGraph.getAtomsByFulltextQuery("\"Arthur Dent\"", f);
        assertEquals(1, result.size());
        assertEquals(a.asVertex().getId(), result.iterator().next().asVertex().getId());
    }

    @Test
    public void testAcronymSearch() throws Exception {
        Filter f = new Filter();

        Atom a = atomGraph.createAtom(f, null);
        a.setValue("Arthur\tP.  Dent ");
        Atom t = atomGraph.createAtom(f, null);
        t.setValue("Arthur's moth-eaten towel");
        Atom l = atomGraph.createAtom(f, null);
        l.setValue("ooooooooo0ooooooooo1ooooooooo2ooooooooo3ooooooooo4ooooooooo5ooooooooo6ooooooooo7" +
                "ooooooooo8ooooooooo9oooooooooAoooooooooBoooooooooCoooooooooDoooooooooEoooooooooF");
        atomGraph.addAtomToIndices(a);
        atomGraph.addAtomToIndices(t);
        atomGraph.addAtomToIndices(l);

        Collection<Atom> result;

        // oops. This is not a full-text query.
        result = atomGraph.getAtomsByAcronymQuery("Arthur*", f);
        assertEquals(0, result.size());

        // l has not been indexed because its value is too long
        result = atomGraph.getAtomsByAcronymQuery("o", f);
        assertEquals(0, result.size());

        // exact acronym match
        // capitalization, punctuation, and idiosyncrasies of white space are ignored
        result = atomGraph.getAtomsByAcronymQuery("apd", f);
        assertEquals(1, result.size());
        assertEquals(a.asVertex().getId(), result.iterator().next().asVertex().getId());

        // hyphens and underscores are treated as white space, while apostrophes and other punctuation are ignored
        result = atomGraph.getAtomsByAcronymQuery("amet", f);
        assertEquals(1, result.size());
        assertEquals(t.asVertex().getId(), result.iterator().next().asVertex().getId());

        // acronym prefix match
        result = atomGraph.getAtomsByAcronymQuery("ap*", f);
        assertEquals(1, result.size());
        assertEquals(a.asVertex().getId(), result.iterator().next().asVertex().getId());

        // acronym search is also case insensitive
        result = atomGraph.getAtomsByAcronymQuery("AP*", f);
        assertEquals(1, result.size());
        assertEquals(a.asVertex().getId(), result.iterator().next().asVertex().getId());
    }
}
