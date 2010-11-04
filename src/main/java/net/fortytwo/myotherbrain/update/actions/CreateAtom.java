package net.fortytwo.myotherbrain.update.actions;

import net.fortytwo.myotherbrain.model.MOB;
import net.fortytwo.myotherbrain.model.concepts.Atom;
import net.fortytwo.myotherbrain.model.concepts.GeoPoint;
import net.fortytwo.myotherbrain.model.concepts.SensitivityLevel;
import net.fortytwo.myotherbrain.model.concepts.WebResource;
import net.fortytwo.myotherbrain.update.NoSuchItemException;
import net.fortytwo.myotherbrain.update.UpdateException;
import net.fortytwo.myotherbrain.update.WriteAction;
import net.fortytwo.myotherbrain.update.WriteContext;

import java.net.URI;
import java.net.URISyntaxException;
import java.util.Date;

/**
 * Author: josh
 * Date: Jun 28, 2009
 * Time: 12:03:59 AM
 */
public class CreateAtom extends WriteAction {

    protected final String name;
    protected final String description;
    protected final String richTextDescription;
    protected final URI icon;
    protected final URI sensitivity;
    protected final Float emphasis;
    protected final Date creationTimeStamp;
    protected final URI creationPlaceStamp;

    // Note: in all "create" actions, the supplied subject URI is expected to be
    // a new, distinct URI with no existing metadata.  Any metadata which is
    // previously attached to the URI may be irreversibly lost in an "undo" event.
    public CreateAtom(
            URI subject,
            String name,
            String description,
            String richTextDescription,
            URI icon,
            URI sensitivity,
            Float emphasis,
            Date creationTimeStamp,
            URI creationPlaceStamp,
            final WriteContext c) throws UpdateException {
        super(subject, c);

        if (null != name) {
            name = c.normalizeName(name);
        }

        if (null != description) {
            description = c.normalizeDescription(description);
        }

        if (null != richTextDescription) {
            richTextDescription = c.normalizeRichTextDescription(richTextDescription);
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

        this.name = name;
        this.description = description;
        this.richTextDescription = richTextDescription;
        this.icon = icon;
        this.sensitivity = sensitivity;
        this.emphasis = emphasis;
        this.creationTimeStamp = creationTimeStamp;
        this.creationPlaceStamp = creationPlaceStamp;
    }

    protected void executeUndo(final WriteContext c) throws UpdateException {
        Atom subject = toThing(this.subject, Atom.class, c);
        c.remove(subject);
    }

    protected void executeRedo(final WriteContext c) throws UpdateException {
        // TODO: is there any reason to use "designate" over "create"?
        Atom subject = c.designate(toQName(this.subject), Atom.class);

        setCommonValues(subject, c);
    }

    protected void setCommonValues(final Atom subject,
                                   final WriteContext c) throws NoSuchItemException {
        if (null != name) {
            subject.setName(name);
        }

        if (null != description) {
            subject.setDescription(description);
        }

        if (null != richTextDescription) {
            subject.setRichTextDescription(richTextDescription);
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
