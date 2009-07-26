package net.fortytwo.myotherbrain.flashmob.query;

import net.fortytwo.myotherbrain.flashmob.model.FlashMOBFirstClassItem;
import net.fortytwo.myotherbrain.model.concepts.Association;
import net.fortytwo.myotherbrain.model.concepts.FirstClassItem;
import net.fortytwo.myotherbrain.query.OntologyBasedQueries;
import net.fortytwo.myotherbrain.query.Handler;
import net.fortytwo.myotherbrain.query.QueryException;

/**
 * Author: josh
 * Date: Jul 25, 2009
 * Time: 6:17:52 PM
 */
public class AssociatedObjectsQuery implements FlashMOBQuery {
    private String subjectURI;

    public String getSubjectURI() {
        return subjectURI;
    }

    public void setSubjectURI(final String subjectURI) {
        this.subjectURI = subjectURI;
    }

    public void execute(final Handler<FlashMOBQueryResult, QueryException> resultHandler,
                        final FlashMOBQueryContext c) throws QueryException {

        Handler<Association, QueryException> handler = new Handler<Association, QueryException>() {
            public boolean handle(final Association a) throws QueryException {
                FirstClassItem obj = a.getObject();

                // For now, only discover an association if both the association and its object are visible.
                if (c.isVisible(a) && c.isVisible(obj)) {
                    // Bean-ify both the Association itself and its object.
                    FlashMOBFirstClassItem aBean = c.toBean(a);
                    FlashMOBFirstClassItem objBean = c.toBean(obj);

                    FlashMOBQueryResult r = new FlashMOBQueryResult();
                    r.setSubject(objBean);
                    // Note: object's emphasis is not taken into account -- at least, not for now
                    r.setEmphasis(a.getEmphasis());
                    r.setSource(aBean);

                    return resultHandler.handle(r);
                } else {
                    return true;
                }
            }
        };

        FirstClassItem subject = c.find(subjectURI);

        if (c.isVisible(subject)) {
            OntologyBasedQueries.handleOutboundAssociations(subject, handler, c.getModelConnection());
        }
    }
}
