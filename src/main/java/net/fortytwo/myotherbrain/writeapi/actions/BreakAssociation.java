package net.fortytwo.myotherbrain.writeapi.actions;

import net.fortytwo.myotherbrain.MOBModelConnection;
import net.fortytwo.myotherbrain.model.beans.Association;
import net.fortytwo.myotherbrain.model.beans.FirstClassItem;

import javax.xml.namespace.QName;
import java.net.URI;

/**
 * Author: josh
 * Date: Jun 28, 2009
 * Time: 12:03:59 AM
 */
public class BreakAssociation extends WriteAction {
    private final URI subject;

    private URI oldAssociationSubject;
    private URI oldAssociationObject;

    public BreakAssociation(final URI subject) {
        if (null == subject) {
            throw new NullPointerException();
        }

        this.subject = subject;
    }

    protected void executeUndo(final MOBModelConnection c) throws NoSuchItemException {
        FirstClassItem item = toEntity(subject, FirstClassItem.class, c);
        c.getElmoManager().removeDesignation(item, FirstClassItem.class);
        c.getElmoManager().designate(new QName(subject.toString()), Association.class);

        Association a = toEntity(subject, Association.class, c);
        a.setSubject(toEntity(oldAssociationSubject, FirstClassItem.class, c));
        a.setObject(toEntity(oldAssociationObject, FirstClassItem.class, c));
    }

    protected void executeRedo(final MOBModelConnection c) throws NoSuchItemException {
        Association a = toEntity(subject, Association.class, c);

        oldAssociationSubject = toURI(a.getSubject());
        oldAssociationObject = toURI(a.getObject());
        a.setSubject(null);
        a.setObject(null);
        
        c.getElmoManager().designate(new QName(subject.toString()), FirstClassItem.class);
        c.getElmoManager().removeDesignation(a, Association.class);
    }
}
