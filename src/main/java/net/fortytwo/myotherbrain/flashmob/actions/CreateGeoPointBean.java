package net.fortytwo.myotherbrain.flashmob.actions;

/**
 * Author: josh
 * Date: Jul 11, 2009
 * Time: 2:19:38 PM
 */
public class CreateGeoPointBean extends ResourceActionBean {
    private Float longitude;
    private Float latitude;

    public Float getLongitude() {
        return longitude;
    }

    public void setLongitude(final Float longitude) {
        this.longitude = longitude;
    }

    public Float getLatitude() {
        return latitude;
    }

    public void setLatitude(final Float latitude) {
        this.latitude = latitude;
    }
}
