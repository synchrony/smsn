package net.fortytwo.myotherbrain.model.beans;

import net.fortytwo.myotherbrain.model.MOB;
import org.openrdf.concepts.owl.Thing;
import org.openrdf.elmo.annotations.rdf;

import javax.xml.datatype.XMLGregorianCalendar;

/**
 * Created by IntelliJ IDEA.
 * User: josh
 * Date: Jan 31, 2009
 * Time: 11:29:19 PM
 * To change this template use File | Settings | File Templates.
 */
@rdf(MOB.NAMESPACE + "FirstClassItem")
public interface FirstClassItem extends Thing {

    @rdf(MOB.NAMESPACE + "markedWith")
    Marker getMarkedWith();

    void setMarkedWith(Marker markedWith);

    @rdf(MOB.NAMESPACE + "name")
    String getName();

    void setName(String name);

    @rdf(MOB.NAMESPACE + "description")
    String getDescription();

    void setDescription(String description);

    @rdf(MOB.NAMESPACE + "sensitivity")
    SensitivityLevel getSensitivity();

    void setSensitivity(SensitivityLevel sensitivity);

    @rdf(MOB.NAMESPACE + "icon")
    WebResourceAlias getIcon();

    void setIcon(WebResourceAlias icon);

    @rdf(MOB.NAMESPACE + "lastModificationTime")
    XMLGregorianCalendar getLastModificationTime();

    void setLastModificationTime(XMLGregorianCalendar lastModificationTime);

    @rdf(MOB.NAMESPACE + "score")
    Float getScore();

    void setScore(Float score);

    @rdf(MOB.NAMESPACE + "emphasis")
    Float getEmphasis();

    void setEmphasis(Float emphasis);

}
