package net.fortytwo.myotherbrain.model.beans;

import net.fortytwo.myotherbrain.model.MOB;
import org.openrdf.concepts.owl.Thing;
import org.openrdf.elmo.annotations.rdf;

/**
 * Author: josh
 * Date: May 7, 2009
 * Time: 7:46:15 PM
 */
@rdf(MOB.NAMESPACE + "SensitivityLevel")
public interface SensitivityLevel extends Thing {
}
