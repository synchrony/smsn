package net.fortytwo.myotherbrain.writeapi.actions;

import net.fortytwo.myotherbrain.model.beans.Association;
import net.fortytwo.myotherbrain.model.beans.FirstClassItem;
import net.fortytwo.myotherbrain.writeapi.WriteAction;
import net.fortytwo.myotherbrain.writeapi.WriteContext;
import net.fortytwo.myotherbrain.writeapi.WriteException;

import javax.xml.namespace.QName;
import java.net.URI;

/**
 * Author: josh
 * Date: Jun 28, 2009
 * Time: 12:03:59 AM
 */
public class BreakAssociation extends WriteAction {

    private URI oldAssociationSubject;
    private URI oldAssociationObject;

    public BreakAssociation(URI subject,
                            final WriteContext c) throws WriteException {
        super(subject, c);
    }

    protected void executeUndo(final WriteContext c) throws WriteException {
        FirstClassItem item = toThing(subject, FirstClassItem.class, c);
        c.removeDesignation(item, FirstClassItem.class);
        c.designate(new QName(subject.toString()), Association.class);

        Association a = toThing(subject, Association.class, c);
        a.setSubject(toThing(oldAssociationSubject, FirstClassItem.class, c));
        a.setObject(toThing(oldAssociationObject, FirstClassItem.class, c));
    }

    protected void executeRedo(final WriteContext c) throws WriteException {
        Association a = toThing(subject, Association.class, c);

        oldAssociationSubject = toURI(a.getSubject());
        oldAssociationObject = toURI(a.getObject());
        a.setSubject(null);
        a.setObject(null);

        c.designate(new QName(subject.toString()), FirstClassItem.class);
        c.removeDesignation(a, Association.class);
    }
}
