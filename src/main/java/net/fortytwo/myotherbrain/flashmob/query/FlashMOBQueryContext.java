package net.fortytwo.myotherbrain.flashmob.query;

import net.fortytwo.myotherbrain.flashmob.FlashMOBSession;
import net.fortytwo.myotherbrain.flashmob.SessionInfo;
import net.fortytwo.myotherbrain.flashmob.model.FlashMOBAssociation;
import net.fortytwo.myotherbrain.flashmob.model.FlashMOBFirstClassItem;
import net.fortytwo.myotherbrain.flashmob.model.FlashMOBLiteral;
import net.fortytwo.myotherbrain.model.MOBModelConnection;
import net.fortytwo.myotherbrain.model.concepts.Association;
import net.fortytwo.myotherbrain.model.concepts.FirstClassItem;
import net.fortytwo.myotherbrain.model.concepts.Literal;
import net.fortytwo.myotherbrain.query.Handler;
import net.fortytwo.myotherbrain.query.QueryException;
import org.openrdf.concepts.owl.Thing;
import org.openrdf.elmo.Entity;

import javax.xml.datatype.XMLGregorianCalendar;
import javax.xml.namespace.QName;
import java.net.URI;
import java.util.Date;

/**
 * Author: josh
 * Date: Jul 25, 2009
 * Time: 6:20:02 PM
 */
public class FlashMOBQueryContext {
    private final MOBModelConnection connection;
    private final FlashMOBSession.SensitivityLevel sensitivityUpperBound;
    private final float emphasisLowerBound;

    public FlashMOBQueryContext(final MOBModelConnection connection,
                                final SessionInfo sessionInfo) {
        this.connection = connection;
        this.sensitivityUpperBound = FlashMOBSession.SensitivityLevel.fromURI(sessionInfo.getSensitivityUpperBound());
        this.emphasisLowerBound = sessionInfo.getEmphasisLowerBound();
    }

    public MOBModelConnection getModelConnection() {
        return connection;
    }

    public FirstClassItem find(final String uri) {
        Entity e = connection.getElmoManager().find(new QName(uri));

        return null == e ? null : (FirstClassItem) e;
    }

    public static FlashMOBFirstClassItem toBean(final FirstClassItem fci) {
        FlashMOBFirstClassItem i;
        if (fci instanceof Association) {
            i = new FlashMOBAssociation();
            ((FlashMOBAssociation) i).setSubjectURI(toString(((Association) fci).getSubject().getQName()));
            ((FlashMOBAssociation) i).setObjectURI(toString(((Association) fci).getObject().getQName()));
        } else if (fci instanceof Literal) {
            i = new FlashMOBLiteral();
            ((FlashMOBLiteral) i).setLexicalForm(((Literal) fci).getLexicalForm());
            ((FlashMOBLiteral) i).setDatatypeURI(toString(((Literal) fci).getDatatypeURI()));
            ((FlashMOBLiteral) i).setLanguageTag(((Literal) fci).getLanguageTag());
        } else {
            i = new FlashMOBFirstClassItem();
        }

        i.setUri(toString(fci.getQName()));
        i.setName(fci.getName());
        i.setDescription(fci.getDescription());
        i.setRichTextDescription(fci.getRichTextDescription());
        i.setIcon(toString(fci.getIcon()));
        i.setSensitivity(toString(fci.getSensitivity()));
        i.setEmphasis(fci.getEmphasis());
        i.setCreationTimeStamp(toDate(fci.getCreationTimeStamp()));

        return i;
    }

    private static String toString(final QName q) {
        return null == q ? null : q.getNamespaceURI() + q.getLocalPart();
    }

    private static String toString(final Thing t) {
        return null == t ? null : toString(t.getQName());
    }

    private static Date toDate(final XMLGregorianCalendar cal) {
        return cal.toGregorianCalendar().getTime();
    }

    private static String toString(final URI u) {
        return null == u ? null : u.toString();
    }

    public boolean isVisible(final FirstClassItem item) {
        return item.getEmphasis() >= emphasisLowerBound
                && !FlashMOBSession.SensitivityLevel.find(item.getSensitivity()).exceeds(this.sensitivityUpperBound);
    }

    public Handler<FirstClassItem, QueryException> createVisibilityFilter(
            final Handler<FirstClassItem, QueryException> subordinateHandler) {
        return new Handler<FirstClassItem, QueryException>() {
            public boolean handle(FirstClassItem item) throws QueryException {
                return !isVisible(item) || subordinateHandler.handle(item);
            }
        };
    }
}
