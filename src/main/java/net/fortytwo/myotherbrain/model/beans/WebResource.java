package net.fortytwo.myotherbrain.model.beans;

import net.fortytwo.myotherbrain.model.MOB;
import org.openrdf.concepts.owl.Thing;
import org.openrdf.elmo.annotations.rdf;

/**
 * Created by IntelliJ IDEA.
 * User: josh
 * Date: Jan 31, 2009
 * Time: 11:29:19 PM
 * To change this template use File | Settings | File Templates.
 */
@rdf(MOB.WEBRESOURCE)
public interface WebResource extends Thing {

    @rdf(MOB.REPRESENTATIONMEDIATYPE)
    String getRepresentationMediaType();

    void setRepresentationMediaType(String representationMediaType);

    @rdf(MOB.REPRESENTATIONSHA1SUM)
    String getRepresentationSha1Sum();

    void setRepresentationSha1Sum(String representationSha1Sum);
}
