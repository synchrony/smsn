package net.fortytwo.myotherbrain.model.beans;

import org.openrdf.elmo.annotations.rdf;
import net.fortytwo.myotherbrain.model.MOB;

/**
 * Created by IntelliJ IDEA.
 * User: josh
 * Date: Jan 31, 2009
 * Time: 11:29:19 PM
 * To change this template use File | Settings | File Templates.
 */
@rdf(MOB.NAMESPACE + "WebResourceAlias")
public interface WebResourceAlias extends ResourceAlias {

    @rdf(MOB.NAMESPACE + "representationSha1Sum")
    String getRepresentationSha1Sum();

    void setRepresentationSha1Sum(String representationSha1Sum);

    @rdf(MOB.NAMESPACE + "representationMediaType")
    String getRepresentationMediaType();

    void setRepresentationMediaType(String representationMediaType);
}
