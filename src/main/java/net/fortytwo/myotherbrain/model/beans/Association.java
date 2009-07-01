package net.fortytwo.myotherbrain.model.beans;

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
public interface Association extends FirstClassItem {
    @rdf(MOB.OBJECT)
    FirstClassItem getObject();

    void setObject(FirstClassItem object);

    @rdf(MOB.SUBJECT)
    FirstClassItem getSubject();

    void setSubject(FirstClassItem subject);
}
