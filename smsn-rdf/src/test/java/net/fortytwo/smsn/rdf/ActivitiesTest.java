package net.fortytwo.smsn.rdf;

import info.aduna.iteration.CloseableIteration;
import net.fortytwo.rdfagents.data.DatasetFactory;
import net.fortytwo.rdfagents.model.Dataset;
import net.fortytwo.rdfagents.model.RDFContentLanguage;
import net.fortytwo.smsn.rdf.vocab.Timeline;
import org.junit.Test;
import org.openrdf.model.Literal;
import org.openrdf.model.Statement;
import org.openrdf.model.URI;
import org.openrdf.model.ValueFactory;
import org.openrdf.model.impl.ValueFactoryImpl;
import org.openrdf.query.BindingSet;
import org.openrdf.query.QueryEvaluationException;
import org.openrdf.query.QueryLanguage;
import org.openrdf.query.impl.EmptyBindingSet;
import org.openrdf.query.parser.ParsedQuery;
import org.openrdf.query.parser.QueryParserUtil;
import org.openrdf.rio.RDFFormat;
import org.openrdf.sail.Sail;
import org.openrdf.sail.SailConnection;
import org.openrdf.sail.memory.MemoryStore;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertFalse;
import static org.junit.Assert.assertTrue;
import static org.junit.Assert.fail;

/**
 * @author Joshua Shinavier (http://fortytwo.net)
 */
public class ActivitiesTest {
    private         ValueFactory valueFactory = new ValueFactoryImpl();
    private String agentUri = "http://example.org/ns#bob";
    private DatasetFactory f = new DatasetFactory();

    @Test
    public void testDatasetForGestureEvent() throws Exception {
        RDFContentLanguage format = null;
        for (RDFContentLanguage l : f.getSupportedLanguages()) {
            if (RDFFormat.NTRIPLES.equals(l.getFormat())) {
                format = l;
            }
        }

        Dataset ds = Activities.datasetForBatonGesture(System.currentTimeMillis(), valueFactory.createURI(agentUri));

        assertEquals(5, ds.getStatements().size());

        if (null != format) {
            f.write(System.out, ds, format);
        }

        Sail sail = new MemoryStore();
        sail.initialize();
        f.addToSail(ds, sail);

        ParsedQuery q = QueryParserUtil.parseQuery(
                QueryLanguage.SPARQL,
                Activities.QUERY_FOR_ALL_GB_GESTURES,
                "http://example.org/baseURI");

        SailConnection sc = sail.getConnection();
        try {
            sc.begin();
            CloseableIteration<? extends BindingSet, QueryEvaluationException> iter
                    = sc.evaluate(q.getTupleExpr(), q.getDataset(), new EmptyBindingSet(), false);
            BindingSet bs = iter.next();
            assertEquals(agentUri, bs.getValue("actor").stringValue());
            assertTrue(bs.getValue("time") instanceof Literal);
            assertFalse(iter.hasNext());
            iter.close();
        } finally {
            sc.close();
        }
    }

    @Test
    public void testDateTimeFormat() throws Exception {

        URI actor = valueFactory.createURI(agentUri);

        long timestamp = 42L;

        Dataset ds = Activities.datasetForHandshakePulse(timestamp, actor);
        for (Statement s : ds.getStatements()) {
            if (s.getPredicate().equals(Timeline.at)) {
                String dateValue = s.getObject().stringValue();
                // we need this millisecond precision in timestamps
                assertTrue(dateValue.contains(":00:00.042"));
                return;
            }
        }

        fail("no " + Timeline.at + " statement in dataset");
    }
}
