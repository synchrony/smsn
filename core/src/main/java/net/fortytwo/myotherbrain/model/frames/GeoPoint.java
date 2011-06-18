package net.fortytwo.myotherbrain.model.frames;

import com.tinkerpop.frames.Property;

/**
 * User: josh
 * Date: 6/16/11
 * Time: 12:06 PM
 */
public interface GeoPoint {
    @Property("latitude")
    Float getLatitude();

    @Property("latitude")
    void setLatitude(Float latitude);

    @Property("longitude")
    Float getLongitude();

    @Property("longitude")
    void setLongitude(Float longitude);

    @Property("altitude")
    Float getAltitude();

    @Property("altitude")
    void setAltitude(Float altitude);
}
