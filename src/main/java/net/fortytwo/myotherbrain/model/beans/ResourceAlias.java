package net.fortytwo.myotherbrain.model.beans;

import net.fortytwo.myotherbrain.model.MOB;
import org.openrdf.elmo.annotations.rdf;

import java.net.URI;

/**
 * Author: josh
 * Date: May 7, 2009
 * Time: 7:44:07 PM
 */
@rdf(MOB.NAMESPACE + "ResourceAlias")
public interface ResourceAlias extends FirstClassItem {
    
    @rdf(MOB.NAMESPACE + "resourceURI")
    URI getResourceURI();

    void setResourceURI(URI resourceURI);
}
