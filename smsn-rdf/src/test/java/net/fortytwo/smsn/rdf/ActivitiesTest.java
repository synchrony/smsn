package net.fortytwo.smsn.rdf;

import org.eclipse.rdf4j.common.iteration.CloseableIteration;
import net.fortytwo.rdfagents.data.DatasetFactory;
import net.fortytwo.rdfagents.model.Dataset;
import net.fortytwo.rdfagents.model.RDFContentLanguage;
import net.fortytwo.smsn.rdf.vocab.Timeline;
import org.junit.Test;
import org.eclipse.rdf4j.model.IRI;
import org.eclipse.rdf4j.model.Literal;
import org.eclipse.rdf4j.model.Statement;
import org.eclipse.rdf4j.model.ValueFactory;
import org.eclipse.rdf4j.model.impl.SimpleValueFactory;
import org.eclipse.rdf4j.query.BindingSet;
import org.eclipse.rdf4j.query.QueryEvaluationException;
import org.eclipse.rdf4j.query.QueryLanguage;
import org.eclipse.rdf4j.query.impl.EmptyBindingSet;
import org.eclipse.rdf4j.query.parser.ParsedQuery;
import org.eclipse.rdf4j.query.parser.QueryParserUtil;
import org.eclipse.rdf4j.rio.RDFFormat;
import org.eclipse.rdf4j.sail.Sail;
import org.eclipse.rdf4j.sail.SailConnection;
import org.eclipse.rdf4j.sail.memory.MemoryStore;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertFalse;
import static org.junit.Assert.assertTrue;
import static org.junit.Assert.fail;

public class ActivitiesTest {
    private final ValueFactory valueFactory = SimpleValueFactory.getInstance();
    private final String agentIri = "http://example.org/ns#bob";
    private final DatasetFactory f = new DatasetFactory();

    @Test
    public void testDatasetForGestureEvent() throws Exception {
        RDFContentLanguage format = null;
        for (RDFContentLanguage l : f.getSupportedLanguages()) {
            if (RDFFormat.NTRIPLES.equals(l.getFormat())) {
                format = l;
            }
        }

        Dataset ds = Activities.datasetForBatonGesture(System.currentTimeMillis(), valueFactory.createIRI(agentIri));

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
                "http://example.org/baseIRI");

        SailConnection sc = sail.getConnection();
        try {
            sc.begin();
            CloseableIteration<? extends BindingSet, QueryEvaluationException> iter
                    = sc.evaluate(q.getTupleExpr(), q.getDataset(), new EmptyBindingSet(), false);
            BindingSet bs = iter.next();
            assertEquals(agentIri, bs.getValue("actor").stringValue());
            assertTrue(bs.getValue("time") instanceof Literal);
            assertFalse(iter.hasNext());
            iter.close();
        } finally {
            sc.close();
        }
    }

    @Test
    public void testDateTimeFormat() throws Exception {

        IRI actor = valueFactory.createIRI(agentIri);

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
