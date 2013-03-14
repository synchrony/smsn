package net.fortytwo.extendo.brain;

import com.tinkerpop.blueprints.impls.neo4j.Neo4jGraph;
import junit.framework.TestCase;
import org.junit.Test;

import java.io.File;
import java.util.Collection;

/**
 * @author Joshua Shinavier (http://fortytwo.net)
 */
public class BrainGraphTest extends TestCase {
    @Test
    public void testFulltextSearch() throws Exception {
        File dir = File.createTempFile("mob", "test");
        dir.delete();
        dir.mkdir();
        dir.deleteOnExit();

        //System.out.println("path: " + dir.getPath());
        Neo4jGraph g = new Neo4jGraph(dir.getPath());
        //g.setMaxBufferSize(1);
        //g.clear();
        try {
            BrainGraph m = BrainGraph.getInstance(g);
            Filter f = new Filter();

            Atom a = m.createAtom(f, null);
            a.setValue("Arthur Dent");
            m.indexForSearch(a, "Arthur Dent");
            //a.setValue("Arthur");

            Collection<Atom> result;

            // Partial searches don't work
            result = m.getAtomsByFulltextQuery("Arthur", f);
            assertEquals(0, result.size());

            // Currently no ability to search on multiple keywords
            result = m.getAtomsByFulltextQuery("Arthur Dent", f);
            assertEquals(0, result.size());

            // Wildcards are supported
            result = m.getAtomsByFulltextQuery("Arthur*", f);
            assertEquals(1, result.size());
            assertEquals(a.asVertex().getId(), result.iterator().next().asVertex().getId());

            // Search is case-insensitive
            result = m.getAtomsByFulltextQuery("arthur*", f);
            assertEquals(1, result.size());

            // Exact matches in quotes
            result = m.getAtomsByFulltextQuery("\"Arthur Dent\"", f);
            assertEquals(1, result.size());
            assertEquals(a.asVertex().getId(), result.iterator().next().asVertex().getId());
        } finally {
            g.shutdown();
        }
    }
}
