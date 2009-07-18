package net.fortytwo.myotherbrain.model.concepts;

import net.fortytwo.myotherbrain.model.MOB;
import org.openrdf.concepts.owl.Thing;
import org.openrdf.elmo.annotations.rdf;

import javax.xml.datatype.XMLGregorianCalendar;
import java.util.Set;

/**
 * Created by IntelliJ IDEA.
 * User: josh
 * Date: Jan 31, 2009
 * Time: 11:29:19 PM
 * To change this template use File | Settings | File Templates.
 */
@rdf(MOB.FIRSTCLASSITEM)
public interface FirstClassItem extends Thing {

    @rdf(MOB.ALIAS)
    Set<Thing> getAlias();

    void setAlias(Set<Thing> alias);

    @rdf(MOB.CREATIONPLACESTAMP)
    GeoPoint getCreationPlaceStamp();

    void setCreationPlaceStamp(GeoPoint creationPlaceStamp);

    @rdf(MOB.CREATIONTIMESTAMP)
    XMLGregorianCalendar getCreationTimeStamp();

    void setCreationTimeStamp(XMLGregorianCalendar creationTimeStamp);

    @rdf(MOB.DESCRIPTION)
    String getDescription();

    void setDescription(String description);

    @rdf(MOB.EMPHASIS)
    Float getEmphasis();

    void setEmphasis(Float emphasis);

    @rdf(MOB.ICON)
    WebResource getIcon();

    void setIcon(WebResource icon);

    @rdf(MOB.MARKERTAG)
    Set<Marker> getMarkerTag();

    void setMarkerTag(Set<Marker> markerTag);

    @rdf(MOB.NAME)
    String getName();

    void setName(String name);

    @rdf(MOB.RICHTEXTDESCRIPTION)
    String getRichTextDescription();

    void setRichTextDescription(String richTextDescription);

    // Note: the mob:score property is not used.  Rather, it serves as a parent
    //       for other, dynamically-defined properties.

    @rdf(MOB.SENSITIVITY)
    SensitivityLevel getSensitivity();

    void setSensitivity(SensitivityLevel sensitivity);
}
