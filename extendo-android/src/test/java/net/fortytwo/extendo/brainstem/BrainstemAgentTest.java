package net.fortytwo.extendo.brainstem;

import info.aduna.iteration.CloseableIteration;
import net.fortytwo.rdfagents.data.DatasetFactory;
import net.fortytwo.rdfagents.model.Dataset;
import net.fortytwo.rdfagents.model.RDFContentLanguage;
import org.junit.Test;
import org.openrdf.model.Literal;
import org.openrdf.query.BindingSet;
import org.openrdf.query.QueryEvaluationException;
import org.openrdf.query.QueryLanguage;
import org.openrdf.query.impl.EmptyBindingSet;
import org.openrdf.query.parser.ParsedQuery;
import org.openrdf.query.parser.QueryParserUtil;
import org.openrdf.sail.Sail;
import org.openrdf.sail.SailConnection;
import org.openrdf.sail.memory.MemoryStore;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertFalse;
import static org.junit.Assert.assertTrue;

/**
 * @author Joshua Shinavier (http://fortytwo.net)
 */
public class BrainstemAgentTest {
    @Test
    public void testDatasetForGestureEvent() throws Exception {
        String agentUri = "http://example.org/ns#bob";
        BrainstemAgent a = new BrainstemAgent(agentUri);

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

        Sail sail = new MemoryStore();
        sail.initialize();
        f.addToSail(ds, sail);

        ParsedQuery q = QueryParserUtil.parseQuery(
                QueryLanguage.SPARQL,
                BrainstemAgent.QUERY_FOR_ALL_GB_GESTURES,
                "http://example.org/baseURI");

        SailConnection sc = sail.getConnection();
        try {
            CloseableIteration<? extends BindingSet, QueryEvaluationException> iter
                    = sc.evaluate(q.getTupleExpr(), q.getDataset(), new EmptyBindingSet(), false);
            BindingSet bs = iter.next();
            assertEquals(agentUri, bs.getValue("person").stringValue());
            assertTrue(bs.getValue("time") instanceof Literal);
            assertFalse(iter.hasNext());
            iter.close();
        } finally {
            sc.close();
        }
    }


}
