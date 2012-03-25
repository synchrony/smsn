package net.fortytwo.myotherbrain;

import com.tinkerpop.blueprints.pgm.impls.neo4j.Neo4jGraph;
import junit.framework.TestCase;
import net.fortytwo.myotherbrain.notes.Filter;

import java.io.File;
import java.util.Collection;

/**
 * @author Joshua Shinavier (http://fortytwo.net)
 */
public class MOBGraphTest extends TestCase {
    public void testFulltextSearch() throws Exception {
        File dir = File.createTempFile("mob", "test");
        dir.delete();
        dir.mkdir();
        dir.deleteOnExit();

        //System.out.println("path: " + dir.getPath());
        Neo4jGraph g = new Neo4jGraph(dir.getPath());
        g.setMaxBufferSize(1);
        g.clear();
        try {
            MOBGraph m = new MOBGraph(g);
            Filter f = new Filter();
            
            Atom a = m.createAtom(f);
            a.setValue("Arthur Dent");

            Collection<Atom> result;
            
            result = m.getAtomsByFulltextQuery("Arthur*", f);
            assertEquals(1, result.size());
            assertEquals(a.asVertex().getId(), result.iterator().next().asVertex().getId());

            // Full-text search is currently case sensitive (although case insensitivity would be preferable)
            result = m.getAtomsByFulltextQuery("arthur*", f);
            assertEquals(0, result.size());

            // Currently ability to search on multiple keywords
            result = m.getAtomsByFulltextQuery("Arthur Dent", f);
            assertEquals(0, result.size());

            // Exact matches in quotes
            result = m.getAtomsByFulltextQuery("\"Arthur Dent\"", f);
            assertEquals(1, result.size());
            assertEquals(a.asVertex().getId(), result.iterator().next().asVertex().getId());
        } finally {
            g.shutdown();
        }
    }
}
