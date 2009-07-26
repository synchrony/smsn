package net.fortytwo.myotherbrain.flashmob.query;

import net.fortytwo.myotherbrain.flashmob.SessionInfo;
import net.fortytwo.myotherbrain.model.MOBModel;
import net.fortytwo.myotherbrain.model.MOBModelConnection;
import net.fortytwo.myotherbrain.query.Handler;
import net.fortytwo.myotherbrain.query.QueryException;

import java.util.Collection;
import java.util.Collections;
import java.util.LinkedList;
import java.util.List;

/**
 * Author: josh
 * Date: Jul 11, 2009
 * Time: 9:32:27 PM
 */
public class FlashMOBQueryExecutor {
    private final MOBModel model;
    private final SessionInfo sessionInfo;

    public FlashMOBQueryExecutor(final MOBModel model,
                                 final SessionInfo sessionInfo) {
        this.model = model;
        this.sessionInfo = sessionInfo;
    }

    public List<FlashMOBQueryResult> execute(final FlashMOBQuery query,
                                             final int startIndex,
                                             final int endIndex) throws QueryException {
        if (startIndex < 0) {
            throw new IllegalArgumentException("start index must be non-negative");
        }

        if (endIndex < 0) {
            throw new IllegalArgumentException("end index must be non-negative");
        }

        List<FlashMOBQueryResult> allResults = new LinkedList<FlashMOBQueryResult>();
        Handler<FlashMOBQueryResult, QueryException> adder
                = new CollectionAdder<FlashMOBQueryResult, QueryException>(allResults);

        MOBModelConnection c = model.createConnection();
        try {
            FlashMOBQueryContext qc = new FlashMOBQueryContext(c, sessionInfo);
            query.execute(adder, qc);
        } finally {
            c.close();
        }

        Collections.sort(allResults);

        if (startIndex >= allResults.size()) {
            return new LinkedList<FlashMOBQueryResult>();
        }

        return allResults.subList(startIndex, endIndex >= allResults.size() ? allResults.size() : endIndex);
    }

    private class CollectionAdder<T, E extends Exception> implements Handler<T, E> {
        private final Collection<T> collection;

        public CollectionAdder(final Collection<T> collection) {
            this.collection = collection;
        }

        public boolean handle(final T t) throws E {
            return collection.add(t);
        }
    }
    /*
    public static List<FlashMOBFirstClassItem> getAllFirstClassItems(final MOBModelConnection c) throws QueryException {
        BeanCollector results = new BeanCollector();
        OntologyBasedQueries.handleAllItems(results, c);
        return results.getResults();
    }

    public static List<FlashMOBAssociation> getObjectAssociations(final String subject,
                                                              final MOBModelConnection c) throws QueryException {
        Collector<FlashMOBAssociation, QueryException> results = new Collector<FlashMOBAssociation, QueryException>();

        OntologyBasedQueries.handleOutboundAssociations(itemFromSubject(subject, c),
                new AssociationToObjectAssociationBeanHandler(results), c);
        return results.toList();
    }

    public static List<FirstClassItemWithAssociatedObjects> getAllItemsWithAssociatedObjects(final MOBModelConnection c) throws QueryException {
        Collector<FirstClassItemWithAssociatedObjects, QueryException> results
                = new Collector<FirstClassItemWithAssociatedObjects, QueryException>();
        OntologyBasedQueries.handleAllItems(new FirstClassItemToFirstClassItemWithAssociatedObjects(results, c), c);
        return results.toList();
    }

    private static FirstClassItem itemFromSubject(final String subject,
                                                  final MOBModelConnection c) {
        return (FirstClassItem) c.getElmoManager().find(new QName(subject));
    }
    */
}
