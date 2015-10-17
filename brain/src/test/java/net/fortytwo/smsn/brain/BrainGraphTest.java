package net.fortytwo.smsn.brain;

import com.tinkerpop.blueprints.impls.neo4j.Neo4jGraph;
import junit.framework.TestCase;
import org.junit.After;
import org.junit.Before;
import org.junit.Test;

import java.io.File;
import java.util.Collection;

/**
 * @author Joshua Shinavier (http://fortytwo.net)
 */
public class BrainGraphTest extends TestCase {
    private Neo4jGraph g;
    private BrainGraph bg;

    @Before
    public void setUp() throws Exception {
        File dir = File.createTempFile("extendo", "test");
        dir.delete();
        dir.mkdir();
        dir.deleteOnExit();

        g = new Neo4jGraph(dir.getPath());

        bg = new BrainGraph(g);
    }

    @After
    public void tearDown() throws Exception {
        g.shutdown();
    }

    @Test
    public void testGetAtoms() throws Exception {
        Filter f = new Filter();

        Atom chaos = bg.createAtom(f, null);
        chaos.setValue("Chaos");
        Atom tartarus = bg.createAtom(f, null);
        tartarus.setValue("Tartarus");
        Atom gaia = bg.createAtom(f, null);
        gaia.setValue("Gaia");
        Atom eros = bg.createAtom(f, null);
        eros.setValue("Eros");
        Atom nyx = bg.createAtom(f, null);
        nyx.setValue("Nyx");
        Atom erebus = bg.createAtom(f, null);
        erebus.setValue("Erebus");
        AtomList children = bg.createAtomList(tartarus, gaia, eros, nyx, erebus);
        chaos.setNotes(children);

        // getAtoms returns a list of only atom vertices, excluding list vertices
        int count = 0;
        Iterable<Atom> atoms = bg.getAtoms();
        for (Atom a : atoms) {
            count++;
            System.out.println(a.getValue());
        }
        assertEquals(6, count);
    }

    @Test
    public void testFulltextSearch() throws Exception {
        Filter f = new Filter();

        Atom a = bg.createAtom(f, null);
        a.setValue("Arthur Dent");
        bg.indexForSearch(a, a.getValue());
        //a.setValue("Arthur");

        Collection<Atom> result;

        // Partial searches don't work
        result = bg.getAtomsByFulltextQuery("Arthur", f);
        assertEquals(0, result.size());

        // Currently no ability to search on multiple keywords
        result = bg.getAtomsByFulltextQuery("Arthur Dent", f);
        assertEquals(0, result.size());

        // Wildcards are supported
        result = bg.getAtomsByFulltextQuery("Arthur*", f);
        assertEquals(1, result.size());
        assertEquals(a.asVertex().getId(), result.iterator().next().asVertex().getId());

        // Search is case-insensitive
        result = bg.getAtomsByFulltextQuery("arthur*", f);
        assertEquals(1, result.size());

        // Exact matches in quotes
        result = bg.getAtomsByFulltextQuery("\"Arthur Dent\"", f);
        assertEquals(1, result.size());
        assertEquals(a.asVertex().getId(), result.iterator().next().asVertex().getId());
    }

    @Test
    public void testAcronymSearch() throws Exception {
        Filter f = new Filter();

        Atom a = bg.createAtom(f, null);
        a.setValue("Arthur\tP.  Dent ");
        Atom t = bg.createAtom(f, null);
        t.setValue("Arthur's moth-eaten towel");
        Atom l = bg.createAtom(f, null);
        l.setValue("ooooooooo0ooooooooo1ooooooooo2ooooooooo3ooooooooo4ooooooooo5ooooooooo6ooooooooo7" +
                "ooooooooo8ooooooooo9oooooooooAoooooooooBoooooooooCoooooooooDoooooooooEoooooooooF");
        bg.indexForSearch(a, a.getValue());
        bg.indexForSearch(t, t.getValue());
        bg.indexForSearch(l, l.getValue());

        Collection<Atom> result;

        // oops. This is not a full-text query.
        result = bg.getAtomsByAcronymQuery("Arthur*", f);
        assertEquals(0, result.size());

        // l has not been indexed because its value is too long
        result = bg.getAtomsByAcronymQuery("o", f);
        assertEquals(0, result.size());

        // exact acronym match
        // capitalization, punctuation, and idiosyncrasies of white space are ignored
        result = bg.getAtomsByAcronymQuery("apd", f);
        assertEquals(1, result.size());
        assertEquals(a.asVertex().getId(), result.iterator().next().asVertex().getId());

        // hyphens and underscores are treated as white space, while apostrophes and other punctuation are ignored
        result = bg.getAtomsByAcronymQuery("amet", f);
        assertEquals(1, result.size());
        assertEquals(t.asVertex().getId(), result.iterator().next().asVertex().getId());

        // acronym prefix match
        result = bg.getAtomsByAcronymQuery("ap*", f);
        assertEquals(1, result.size());
        assertEquals(a.asVertex().getId(), result.iterator().next().asVertex().getId());

        // acronym search is also case insensitive
        result = bg.getAtomsByAcronymQuery("AP*", f);
        assertEquals(1, result.size());
        assertEquals(a.asVertex().getId(), result.iterator().next().asVertex().getId());


    }
}
