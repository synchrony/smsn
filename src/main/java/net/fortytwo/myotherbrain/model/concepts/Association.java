package net.fortytwo.myotherbrain.model.concepts;

import net.fortytwo.myotherbrain.model.MOBOntology;
import org.openrdf.elmo.annotations.rdf;

/**
 * Created by IntelliJ IDEA.
 * User: josh
 * Date: Jan 31, 2009
 * Time: 11:25:26 PM
 * To change this template use File | Settings | File Templates.
 */
@rdf(MOBOntology.ASSOCIATION)
public interface Association extends Atom {
    @rdf(MOBOntology.OBJECT)
    Atom getObject();

    void setObject(Atom object);

    @rdf(MOBOntology.SUBJECT)
    Atom getSubject();

    void setSubject(Atom subject);
}
