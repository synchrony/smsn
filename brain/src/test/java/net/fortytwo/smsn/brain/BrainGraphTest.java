package net.fortytwo.smsn.brain;

import com.tinkerpop.blueprints.impls.neo4j.Neo4jGraph;
import org.junit.After;
import org.junit.Before;
import org.junit.Test;

import java.io.File;
import java.util.Collection;

import static org.junit.Assert.assertEquals;

/**
 * @author Joshua Shinavier (http://fortytwo.net)
 */
public class BrainGraphTest {
    private Neo4jGraph graph;
    private BrainGraph brainGraph;

    @Before
    public void setUp() throws Exception {
        File dir = File.createTempFile("extendo", "test");
        dir.delete();
        dir.mkdir();
        dir.deleteOnExit();

        graph = new Neo4jGraph(dir.getPath());

        brainGraph = new BrainGraph(graph);
    }

    @After
    public void tearDown() throws Exception {
        graph.shutdown();
    }

    @Test
    public void testGetAtoms() throws Exception {
        Filter f = new Filter();

        Atom chaos = brainGraph.createAtom(f, null);
        chaos.setValue("Chaos");
        Atom tartarus = brainGraph.createAtom(f, null);
        tartarus.setValue("Tartarus");
        Atom gaia = brainGraph.createAtom(f, null);
        gaia.setValue("Gaia");
        Atom eros = brainGraph.createAtom(f, null);
        eros.setValue("Eros");
        Atom nyx = brainGraph.createAtom(f, null);
        nyx.setValue("Nyx");
        Atom erebus = brainGraph.createAtom(f, null);
        erebus.setValue("Erebus");
        AtomList children = brainGraph.createAtomList(tartarus, gaia, eros, nyx, erebus);
        chaos.setNotes(children);

        // getAtoms returns a list of only atom vertices, excluding list vertices
        int count = 0;
        Iterable<Atom> atoms = brainGraph.getAtoms();
        for (Atom a : atoms) {
            count++;
            System.out.println(a.getValue());
        }
        assertEquals(6, count);
    }

    @Test
    public void testFulltextSearch() throws Exception {
        Filter f = new Filter();

        Atom a = brainGraph.createAtom(f, null);
        a.setValue("Arthur Dent");
        brainGraph.indexForSearch(a, a.getValue());
        //a.setValue("Arthur");

        Collection<Atom> result;

        // Partial searches don't work
        result = brainGraph.getAtomsByFulltextQuery("Arthur", f);
        assertEquals(0, result.size());

        // Currently no ability to search on multiple keywords
        result = brainGraph.getAtomsByFulltextQuery("Arthur Dent", f);
        assertEquals(0, result.size());

        // Wildcards are supported
        result = brainGraph.getAtomsByFulltextQuery("Arthur*", f);
        assertEquals(1, result.size());
        assertEquals(a.asVertex().getId(), result.iterator().next().asVertex().getId());

        // Search is case-insensitive
        result = brainGraph.getAtomsByFulltextQuery("arthur*", f);
        assertEquals(1, result.size());

        // Exact matches in quotes
        result = brainGraph.getAtomsByFulltextQuery("\"Arthur Dent\"", f);
        assertEquals(1, result.size());
        assertEquals(a.asVertex().getId(), result.iterator().next().asVertex().getId());
    }

    @Test
    public void testAcronymSearch() throws Exception {
        Filter f = new Filter();

        Atom a = brainGraph.createAtom(f, null);
        a.setValue("Arthur\tP.  Dent ");
        Atom t = brainGraph.createAtom(f, null);
        t.setValue("Arthur's moth-eaten towel");
        Atom l = brainGraph.createAtom(f, null);
        l.setValue("ooooooooo0ooooooooo1ooooooooo2ooooooooo3ooooooooo4ooooooooo5ooooooooo6ooooooooo7" +
                "ooooooooo8ooooooooo9oooooooooAoooooooooBoooooooooCoooooooooDoooooooooEoooooooooF");
        brainGraph.indexForSearch(a, a.getValue());
        brainGraph.indexForSearch(t, t.getValue());
        brainGraph.indexForSearch(l, l.getValue());

        Collection<Atom> result;

        // oops. This is not a full-text query.
        result = brainGraph.getAtomsByAcronymQuery("Arthur*", f);
        assertEquals(0, result.size());

        // l has not been indexed because its value is too long
        result = brainGraph.getAtomsByAcronymQuery("o", f);
        assertEquals(0, result.size());

        // exact acronym match
        // capitalization, punctuation, and idiosyncrasies of white space are ignored
        result = brainGraph.getAtomsByAcronymQuery("apd", f);
        assertEquals(1, result.size());
        assertEquals(a.asVertex().getId(), result.iterator().next().asVertex().getId());

        // hyphens and underscores are treated as white space, while apostrophes and other punctuation are ignored
        result = brainGraph.getAtomsByAcronymQuery("amet", f);
        assertEquals(1, result.size());
        assertEquals(t.asVertex().getId(), result.iterator().next().asVertex().getId());

        // acronym prefix match
        result = brainGraph.getAtomsByAcronymQuery("ap*", f);
        assertEquals(1, result.size());
        assertEquals(a.asVertex().getId(), result.iterator().next().asVertex().getId());

        // acronym search is also case insensitive
        result = brainGraph.getAtomsByAcronymQuery("AP*", f);
        assertEquals(1, result.size());
        assertEquals(a.asVertex().getId(), result.iterator().next().asVertex().getId());
    }
}
