package net.fortytwo.myotherbrain;

import net.fortytwo.myotherbrain.model.concepts.Atom;
import net.fortytwo.myotherbrain.model.MOBModelConnection;
import org.openrdf.model.URI;

import java.io.InputStream;

/**
 * Author: josh
 * Date: May 7, 2009
 * Time: 3:35:52 AM
 */
public class WebDAVPrototype {

    /**
     * Both pushes a resource to the Web via WebDAV and (if this is successful), adds corresponding metadata to the knowledge base.
     *
     * @param resourceURI
     * @param content
     * @param mediaType
     */
    // TODO: use the appropriate class for mediaType
    // TODO: use a more specific return type than Atom
    public Atom pushResource(final URI resourceURI,
                             final InputStream content,
                             final String mediaType,
                             final MOBModelConnection modelConnection) {
        return null;
    }


}
