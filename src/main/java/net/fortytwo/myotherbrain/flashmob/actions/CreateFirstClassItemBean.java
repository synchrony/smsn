package net.fortytwo.myotherbrain.flashmob.actions;

import java.net.URI;
import java.util.Date;

/**
 * Author: josh
 * Date: Jul 11, 2009
 * Time: 2:14:19 PM
 */
public class CreateFirstClassItemBean extends SubjectBean {

    protected String name;
    protected String description;
    protected String icon;
    protected String sensitivity;
    protected Float emphasis;
    protected Date creationTimeStamp;
    protected String creationPlaceStamp;

    public String getName() {
        return name;
    }

    public void setName(final String name) {
        this.name = name;
    }

    public String getDescription() {
        return description;
    }

    public void setDescription(final String description) {
        this.description = description;
    }

    public String getIcon() {
        return icon;
    }

    public void setIcon(final String icon) {
        this.icon = icon;
    }

    public String getSensitivity() {
        return sensitivity;
    }

    public void setSensitivity(final String sensitivity) {
        this.sensitivity = sensitivity;
    }

    public Float getEmphasis() {
        return emphasis;
    }

    public void setEmphasis(final Float emphasis) {
        this.emphasis = emphasis;
    }

    public Date getCreationTimeStamp() {
        return creationTimeStamp;
    }

    public void setCreationTimeStamp(final Date creationTimeStamp) {
        this.creationTimeStamp = creationTimeStamp;
    }

    public String getCreationPlaceStamp() {
        return creationPlaceStamp;
    }

    public void setCreationPlaceStamp(final String creationPlaceStamp) {
        this.creationPlaceStamp = creationPlaceStamp;
    }
}
