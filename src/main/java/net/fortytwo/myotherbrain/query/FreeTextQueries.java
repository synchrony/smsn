package net.fortytwo.myotherbrain.query;

import info.aduna.iteration.CloseableIteration;
import net.fortytwo.myotherbrain.model.MOBModelConnection;
import net.fortytwo.myotherbrain.model.concepts.Atom;
import org.neo4j.rdf.sail.FulltextQueryResult;
import org.neo4j.rdf.sail.NeoRdfSailConnection;
import org.openrdf.elmo.Entity;
import org.openrdf.model.Resource;
import org.openrdf.model.Statement;
import org.openrdf.model.URI;
import org.openrdf.sail.SailConnection;
import org.openrdf.sail.SailException;

import javax.xml.namespace.QName;
import java.util.Arrays;

/**
 * Author: josh
 * Date: Jul 15, 2009
 * Time: 6:26:50 PM
 */
public class FreeTextQueries {
    public static void executeFreetextSearch(final String query,
                                             final Handler<WeightedValue<Atom>, QueryException> handler,
                                             final MOBModelConnection c) throws QueryException {
        if (!c.freeTextSearchSupported()) {
            return;
        }

        WeightedVector<URI> matches = new WeightedVector<URI>();

        try {
            // We need the full text Sail rather than the main Sail.
            final SailConnection sc = c.createSailConnection();

            try {
                CloseableIteration<? extends FulltextQueryResult, SailException> fulltextIter
                        = ((NeoRdfSailConnection) sc).evaluate(query);
                try {
                    // Note: iterates eagerly, through all results.  Lazy iteration is
                    // preferable.
                    //for (FulltextQueryResult ft : toCollection(fulltextIter)) {
                    while (fulltextIter.hasNext()) {
                        FulltextQueryResult ft = fulltextIter.next();

                        Statement st = ft.getStatement();
                        Resource subj = st.getSubject();
                        final double weight = ft.getScore();

                        if (subj instanceof URI) {
                            // Add the direct match.
                            matches.addWeight((URI) subj, weight);

                            // TODO (maybe): indirect matches
                        }
                    }
                } finally {
                    fulltextIter.close();
                }
            } finally {
                sc.close();
            }
        } catch (SailException e) {
            throw new QueryException(e);
        }

        WeightedValue<URI>[] values = new WeightedValue[matches.size()];
        matches.values().toArray(values);
        Arrays.sort(values);
        for (int i = values.length - 1; i >= 0; i--) {
            WeightedValue<URI> wv = values[i];
            URI u = wv.value;

            Entity item = c.getElmoManager().find(new QName(u.toString()));
            if (null != item && item instanceof Atom) {
                WeightedValue<Atom> f = new WeightedValue<Atom>();
                f.weight = wv.weight;
                f.value = (Atom) item;
                handler.handle(f);
            }
        }
    }
}
