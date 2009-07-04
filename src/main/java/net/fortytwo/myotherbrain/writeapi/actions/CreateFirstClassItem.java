package net.fortytwo.myotherbrain.writeapi.actions;

import net.fortytwo.myotherbrain.model.MOB;
import net.fortytwo.myotherbrain.model.beans.FirstClassItem;
import net.fortytwo.myotherbrain.model.beans.GeoPoint;
import net.fortytwo.myotherbrain.model.beans.SensitivityLevel;
import net.fortytwo.myotherbrain.model.beans.WebResource;
import net.fortytwo.myotherbrain.writeapi.NoSuchItemException;
import net.fortytwo.myotherbrain.writeapi.WriteAction;
import net.fortytwo.myotherbrain.writeapi.WriteContext;
import net.fortytwo.myotherbrain.writeapi.WriteException;

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
            URI subject,
            String name,
            String description,
            URI icon,
            URI sensitivity,
            Float emphasis,
            Date creationTimeStamp,
            URI creationPlaceStamp,
            final WriteContext c) throws WriteException {
        if (null == subject) {
            throw new NullPointerException();
        } else {
            subject = c.normalizeResourceURI(subject);
        }

        if (null != name) {
            name = c.normalizeName(name);
        }

        if (null != description) {
            description = c.normalizeDescription(description);
        }

        if (null != icon) {
            icon = c.normalizeResourceURI(icon);
        }

        if (null == sensitivity) {
            try {
                sensitivity = new URI(MOB.PERSONAL);
            } catch (URISyntaxException e) {
                throw new IllegalStateException();
            }
        } else {
            sensitivity = c.normalizeResourceURI(sensitivity);
        }

        if (null != emphasis) {
            emphasis = c.normalizeEmphasis(emphasis);
        }

        if (null != creationTimeStamp) {
            creationTimeStamp = c.normalizeCreationTimeStamp(creationTimeStamp);
        }

        if (null != creationPlaceStamp) {
            creationPlaceStamp = c.normalizeResourceURI(creationPlaceStamp);
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

    protected void executeUndo(final WriteContext c) throws WriteException {
        FirstClassItem subject = toThing(this.subject, FirstClassItem.class, c);
        c.remove(subject);
    }

    protected void executeRedo(final WriteContext c) throws WriteException {
        // TODO: is there any reason to use "designate" over "create"?
        FirstClassItem subject = c.designate(toQName(this.subject), FirstClassItem.class);

        setCommonValues(subject, c);
    }

    protected void setCommonValues(final FirstClassItem subject,
                                   final WriteContext c) throws NoSuchItemException {
        if (null != name) {
            subject.setName(name);
        }

        if (null != description) {
            subject.setDescription(description);
        }

        if (null != icon) {
            subject.setIcon(toThing(icon, WebResource.class, c));
        }

        subject.setSensitivity(toThing(sensitivity, SensitivityLevel.class, c));

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
