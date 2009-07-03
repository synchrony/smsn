package net.fortytwo.myotherbrain.writeapi.actions;

import net.fortytwo.myotherbrain.MOBModelConnection;
import net.fortytwo.myotherbrain.model.beans.GeoPoint;

import java.net.URI;

/**
 * Author: josh
 * Date: Jun 28, 2009
 * Time: 12:03:59 AM
 */
public class CreateGeoPoint extends WriteAction {

    protected final URI subject;
    protected final Float longitude;
    protected final Float latitude;

    public CreateGeoPoint(
            final URI subject,
            final Float longitude,
            final Float latitude) {
        if (null == subject) {
            throw new NullPointerException();
        }

        if (null == longitude) {
            throw new NullPointerException();
        }

        if (null == latitude) {
            throw new NullPointerException();
        }

        this.subject = subject;
        this.longitude = longitude;
        this.latitude = latitude;
    }

    protected void executeUndo(final MOBModelConnection c) throws NoSuchItemException {
        GeoPoint subject = toThing(this.subject, GeoPoint.class, c);
        c.getElmoManager().remove(subject);
    }

    protected void executeRedo(final MOBModelConnection c) throws NoSuchItemException {
        // TODO: is there any reason to use "designate" over "create"?
        GeoPoint subject = c.getElmoManager().designate(toQName(this.subject), GeoPoint.class);
        subject.setLongitude(longitude);
        subject.setLatitude(latitude);
    }
}
