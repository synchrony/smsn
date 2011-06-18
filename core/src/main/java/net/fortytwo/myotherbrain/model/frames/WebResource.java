package net.fortytwo.myotherbrain.model.frames;

import com.tinkerpop.frames.Property;

/**
 * User: josh
 * Date: 5/5/11
 * Time: 3:38 PM
 */
public interface WebResource {
    @Property("url")
    String getUrl();

    @Property("url")
    void setUrl(String url);

    @Property("representationMediaType")
    String getRepresentationMediaType();

    @Property("representationMediaType")
    void setRepresentationMediaType(String representationMediaType);

    @Property("representationChecksum")
    String getRepresentationChecksum();

    @Property("representationChecksum")
    void setRepresentationChecksum(String representationChecksum);
}
