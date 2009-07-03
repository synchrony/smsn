package net.fortytwo.myotherbrain.writeapi.actions;

import net.fortytwo.myotherbrain.MOBModelConnection;
import net.fortytwo.myotherbrain.model.beans.FirstClassItem;
import net.fortytwo.myotherbrain.model.beans.WebResource;
import net.fortytwo.myotherbrain.model.beans.SensitivityLevel;
import net.fortytwo.myotherbrain.model.beans.GeoPoint;
import net.fortytwo.myotherbrain.model.MOB;

import java.net.URI;
import java.net.URISyntaxException;
import java.util.Date;

/**
 * Author: josh
 * Date: Jun 28, 2009
 * Time: 12:03:59 AM
 */
public class CreateFirstClassItem extends WriteAction {

    protected final URI subject;
    protected final String name;
    protected final String description;
    protected final URI icon;
    protected final URI sensitivity;
    protected final Float emphasis;
    protected final Date creationTimeStamp;
    protected final URI creationPlaceStamp;

    // Note: in all "create" actions, the supplied subject URI is expected to be
    // a new, distinct URI with no existing metadata.  Any metadata which is
    // already attached to the URI may be irreversibly lost in an "undo" event.
    public CreateFirstClassItem(
            final URI subject,
            final String name,
            final String description,
            final URI icon,
            final URI sensitivity,
            final Float emphasis,
            final Date creationTimeStamp,
            final URI creationPlaceStamp) {
        if (null == subject) {
            throw new NullPointerException();
        }

        this.subject = subject;
        this.name = name;
        this.description = description;
        this.icon = icon;
        this.sensitivity = sensitivity;
        this.emphasis = emphasis;
        this.creationTimeStamp = creationTimeStamp;
        this.creationPlaceStamp = creationPlaceStamp;
    }

    protected void executeUndo(final MOBModelConnection c) throws NoSuchItemException {
        FirstClassItem subject = toThing(this.subject, FirstClassItem.class, c);
        c.getElmoManager().remove(subject);
    }

    protected void executeRedo(final MOBModelConnection c) throws NoSuchItemException {
        // TODO: is there any reason to use "designate" over "create"?
        FirstClassItem subject = c.getElmoManager().designate(toQName(this.subject), FirstClassItem.class);

        setCommonValues(subject, c);
    }

    protected void setCommonValues(final FirstClassItem subject,
                                   final MOBModelConnection c) throws NoSuchItemException {
        if (null != name) {
            subject.setName(name);
        }

        if (null != description) {
            subject.setDescription(description);
        }

        if (null != icon) {
            subject.setIcon(toThing(icon, WebResource.class, c));
        }

        try {
            // Use default sensitivity level if not specified.
            URI sl = null == sensitivity
                    ? new URI(MOB.PERSONAL)
                    : sensitivity;
            subject.setSensitivity(toThing(sl, SensitivityLevel.class, c));
        } catch (URISyntaxException e) {
            throw new IllegalStateException();
        }

        if (null != emphasis) {
            subject.setEmphasis(emphasis);
        }

        if (null != creationTimeStamp) {
            subject.setCreationTimeStamp(toXMLGregorianCalendar(creationTimeStamp));
        }

        if (null != creationPlaceStamp) {
            GeoPoint p = toThing(creationPlaceStamp, GeoPoint.class, c);
            subject.setCreationPlaceStamp(p);
        }
    }
}
