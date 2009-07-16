package net.fortytwo.myotherbrain.update.actions;

import net.fortytwo.myotherbrain.model.concepts.Association;
import net.fortytwo.myotherbrain.model.concepts.FirstClassItem;
import net.fortytwo.myotherbrain.update.WriteAction;
import net.fortytwo.myotherbrain.update.WriteContext;
import net.fortytwo.myotherbrain.update.UpdateException;

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
                            final WriteContext c) throws UpdateException {
        super(subject, c);
    }

    protected void executeUndo(final WriteContext c) throws UpdateException {
        FirstClassItem item = toThing(subject, FirstClassItem.class, c);
        c.removeDesignation(item, FirstClassItem.class);
        c.designate(new QName(subject.toString()), Association.class);

        Association a = toThing(subject, Association.class, c);
        a.setSubject(toThing(oldAssociationSubject, FirstClassItem.class, c));
        a.setObject(toThing(oldAssociationObject, FirstClassItem.class, c));
    }

    protected void executeRedo(final WriteContext c) throws UpdateException {
        Association a = toThing(subject, Association.class, c);

        oldAssociationSubject = toURI(a.getSubject());
        oldAssociationObject = toURI(a.getObject());
        a.setSubject(null);
        a.setObject(null);

        c.designate(new QName(subject.toString()), FirstClassItem.class);
        c.removeDesignation(a, Association.class);
    }
}
