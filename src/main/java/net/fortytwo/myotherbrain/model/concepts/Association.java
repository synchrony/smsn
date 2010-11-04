package net.fortytwo.myotherbrain.model.concepts;

import net.fortytwo.myotherbrain.model.MOB;
import org.openrdf.elmo.annotations.rdf;

/**
 * Created by IntelliJ IDEA.
 * User: josh
 * Date: Jan 31, 2009
 * Time: 11:25:26 PM
 * To change this template use File | Settings | File Templates.
 */
@rdf(MOB.ASSOCIATION)
public interface Association extends Atom {
    @rdf(MOB.OBJECT)
    Atom getObject();

    void setObject(Atom object);

    @rdf(MOB.SUBJECT)
    Atom getSubject();

    void setSubject(Atom subject);
}
