package net.fortytwo.myotherbrain.model.beans;

import net.fortytwo.myotherbrain.model.MOB;
import org.openrdf.concepts.owl.Thing;
import org.openrdf.elmo.annotations.rdf;

/**
 * Author: josh
 * Date: May 7, 2009
 * Time: 7:37:43 PM
 */
@rdf(MOB.NAMESPACE + "Graph")
public interface Graph extends Thing {
}
