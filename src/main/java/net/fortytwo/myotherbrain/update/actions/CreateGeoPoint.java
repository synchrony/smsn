package net.fortytwo.myotherbrain.update.actions;

import net.fortytwo.myotherbrain.model.concepts.GeoPoint;
import net.fortytwo.myotherbrain.update.WriteAction;
import net.fortytwo.myotherbrain.update.WriteContext;
import net.fortytwo.myotherbrain.update.UpdateException;

import java.net.URI;

/**
 * Author: josh
 * Date: Jun 28, 2009
 * Time: 12:03:59 AM
 */
public class CreateGeoPoint extends WriteAction {

    protected final Float longitude;
    protected final Float latitude;

    public CreateGeoPoint(
             URI subject,
             Float longitude,
             Float latitude,
                    final WriteContext c) throws UpdateException {
        super(subject, c);

        if (null == longitude) {
            throw new NullPointerException();
        } else {
            longitude = c.normalizeLongitude(longitude);
        }

        if (null == latitude) {
            throw new NullPointerException();
        } else {
            latitude = c.normalizeLatitude(latitude);
        }

        this.longitude = longitude;
        this.latitude = latitude;
    }

    protected void executeUndo(final WriteContext c) throws UpdateException {
        GeoPoint subject = toThing(this.subject, GeoPoint.class, c);
        c.remove(subject);
    }

    protected void executeRedo(final WriteContext c) throws UpdateException {
        // TODO: is there any reason to use "designate" over "create"?
        GeoPoint subject = c.designate(toQName(this.subject), GeoPoint.class);
        subject.setLongitude(longitude);
        subject.setLatitude(latitude);
    }
}
