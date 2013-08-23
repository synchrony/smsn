package net.fortytwo.extendo.brain;

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

        bg = BrainGraph.getInstance(g);
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
        bg.indexForSearch(a, "Arthur Dent");
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
}
