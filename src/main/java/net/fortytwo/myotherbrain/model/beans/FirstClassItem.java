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
@rdf(MOB.NAMESPACE + "FirstClassItem")
public interface FirstClassItem {
    @rdf(MOB.NAMESPACE + "weight")
    Float getWeight();

    void setWeight(Float weight);

    @rdf(MOB.NAMESPACE + "title")
    String getTitle();

    void setTitle(String title);

    @rdf(MOB.NAMESPACE + "description")
    String getDescription();

    void setDescription(String description);
}
