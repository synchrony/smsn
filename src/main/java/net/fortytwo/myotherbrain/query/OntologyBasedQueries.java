package net.fortytwo.myotherbrain.query;

import net.fortytwo.myotherbrain.model.MOBOntology;
import net.fortytwo.myotherbrain.model.MOBModelConnection;
import net.fortytwo.myotherbrain.model.concepts.Association;
import net.fortytwo.myotherbrain.model.concepts.Atom;
import org.openrdf.elmo.ElmoQuery;
import org.openrdf.model.vocabulary.RDF;

import java.util.Iterator;

/**
 * Author: josh
 * Date: Jul 14, 2009
 * Time: 6:56:33 PM
 */
public class OntologyBasedQueries {
    private static final String
            SELECT_ITEMS = "SELECT ?item\n"
            + " WHERE { ?item <" + RDF.TYPE + "> <" + MOBOntology.ATOM + "> . }",
            SELECT_OUTBOUND_ASSOCIATIONS = "SELECT ?ass\n"
                    + " WHERE { ?ass <" + MOBOntology.SUBJECT + "> ?subject . }";

    // Note: no inference
    public static <E extends Exception> void handleAllItems(final Handler<Atom, E> handler,
                                                            final MOBModelConnection c) throws E {
        ElmoQuery query = c.getElmoManager().createQuery(SELECT_ITEMS);
        handleResults(query, handler);
    }

    public static <E extends Exception> void handleOutboundAssociations(final Atom subject,
                                                                        final Handler<Association, E> handler,
                                                                        final MOBModelConnection c) throws E {
        ElmoQuery query = c.getElmoManager().createQuery(SELECT_OUTBOUND_ASSOCIATIONS);
        query.setParameter("subject", subject);
        handleResults(query, handler);
    }

    ////////////////////////////////////////////////////////////////////////////

    private static <T, E extends Exception> void handleResults(final ElmoQuery query,
                                                               final Handler<T, E> handler) throws E {
        for (Iterator<T> iter = (Iterator<T>) query.evaluate(); iter.hasNext();) {
            if (!handler.handle(iter.next())) {
                break;
            }
        }
    }
}
