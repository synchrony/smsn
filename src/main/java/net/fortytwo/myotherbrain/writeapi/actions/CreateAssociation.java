package net.fortytwo.myotherbrain.writeapi.actions;

import net.fortytwo.myotherbrain.MOBModelConnection;
import net.fortytwo.myotherbrain.model.beans.Association;
import net.fortytwo.myotherbrain.model.beans.FirstClassItem;

import java.net.URI;
import java.util.Date;

/**
 * Author: josh
 * Date: Jun 28, 2009
 * Time: 12:03:59 AM
 */
public class CreateAssociation extends CreateFirstClassItem {
    private final URI associationSubject;
    private final URI associationObject;

    public CreateAssociation(
            final URI subject,
            final String name,
            final String description,
            final URI icon,
            final URI sensitivity,
            final Float emphasis,
            final Date creationTimeStamp,
            final URI creationPlaceStamp,
            final URI associationSubject,
            final URI associationObject) {
        super(subject, name, description, icon, sensitivity, emphasis,
                creationTimeStamp, creationPlaceStamp);
        if (null == associationSubject) {
            throw new NullPointerException();
        }

        if (null == associationObject) {
            throw new NullPointerException();
        }

        this.associationSubject = associationSubject;
        this.associationObject = associationObject;
    }

    protected void executeUndo(final MOBModelConnection c) throws NoSuchItemException {
        FirstClassItem subject = toThing(this.subject, FirstClassItem.class, c);
        c.getElmoManager().remove(subject);
    }

    protected void executeRedo(final MOBModelConnection c) throws NoSuchItemException {
        // TODO: is there any reason to use "designate" over "create"?
        Association subject = c.getElmoManager().designate(toQName(this.subject), Association.class);

        setCommonValues(subject, c);

        subject.setSubject(toThing(associationSubject, FirstClassItem.class, c));
        subject.setObject(toThing(associationObject, FirstClassItem.class, c));
    }
}
