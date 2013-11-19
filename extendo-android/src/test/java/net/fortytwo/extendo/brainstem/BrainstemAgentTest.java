package net.fortytwo.extendo.brainstem;

import net.fortytwo.rdfagents.data.DatasetFactory;
import net.fortytwo.rdfagents.model.Dataset;
import net.fortytwo.rdfagents.model.RDFContentLanguage;
import org.junit.Test;

import static org.junit.Assert.assertEquals;

/**
 * @author Joshua Shinavier (http://fortytwo.net)
 */
public class BrainstemAgentTest {
    @Test
    public void testDatasetForGestureEvent() throws Exception {
        BrainstemAgent a = new BrainstemAgent("http://example.org/ns#bob");

        DatasetFactory f = a.getDatasetFactory();

        //System.out.println("available RDF content languages:");
        RDFContentLanguage turtle = null;
        for (RDFContentLanguage l : f.getSupportedLanguages()) {
            if ("rdf-turtle".equals(l.getFipaName())) {
                turtle = l;
            }
            //    System.out.println("\t" + l.getFipaName() + ": " + l.getFormat().getName());
        }

        Dataset ds = a.datasetForGestureEvent(System.currentTimeMillis());

        assertEquals(5, ds.getStatements().size());

        if (null != turtle) {
            f.write(System.out, ds, turtle);
        }
    }
}
