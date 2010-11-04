package net.fortytwo.myotherbrain.model.concepts;

import net.fortytwo.myotherbrain.model.MOBOntology;
import org.openrdf.concepts.owl.Thing;
import org.openrdf.elmo.annotations.rdf;

/**
 * Author: josh
 * Date: May 7, 2009
 * Time: 7:53:04 PM
 */
@rdf(MOBOntology.GEOPOINT)
public interface GeoPoint extends Thing {

    @rdf(MOBOntology.LATITUDE)
    Float getLatitude();

    void setLatitude(Float latitude);

    @rdf(MOBOntology.LONGITUDE)
    Float getLongitude();

    void setLongitude(Float longitude);
}
