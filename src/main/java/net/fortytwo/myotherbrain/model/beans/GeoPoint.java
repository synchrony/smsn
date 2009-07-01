package net.fortytwo.myotherbrain.model.beans;

import net.fortytwo.myotherbrain.model.MOB;
import org.openrdf.concepts.owl.Thing;
import org.openrdf.elmo.annotations.rdf;

/**
 * Author: josh
 * Date: May 7, 2009
 * Time: 7:53:04 PM
 */
@rdf(MOB.GEOPOINT)
public interface GeoPoint extends Thing {

    @rdf(MOB.LATITUDE)
    Float getLatitude();

    void setLatitude(Float latitude);

    @rdf(MOB.LONGITUDE)
    Float getLongitude();

    void setLongitude(Float longitude);
}
