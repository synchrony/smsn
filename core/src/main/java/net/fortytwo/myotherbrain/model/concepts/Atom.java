package net.fortytwo.myotherbrain.model.concepts;

import net.fortytwo.myotherbrain.model.MOBOntology;
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
@rdf(MOBOntology.ATOM)
public interface Atom extends Thing {

    @rdf(MOBOntology.ALIAS)
    Set<Thing> getAlias();

    void setAlias(Set<Thing> alias);

    @rdf(MOBOntology.CREATIONPLACESTAMP)
    GeoPoint getCreationPlaceStamp();

    void setCreationPlaceStamp(GeoPoint creationPlaceStamp);

    @rdf(MOBOntology.CREATIONTIMESTAMP)
    XMLGregorianCalendar getCreationTimeStamp();

    void setCreationTimeStamp(XMLGregorianCalendar creationTimeStamp);

    @rdf(MOBOntology.DESCRIPTION)
    String getDescription();

    void setDescription(String description);

    @rdf(MOBOntology.EMPHASIS)
    Float getEmphasis();

    void setEmphasis(Float emphasis);

    @rdf(MOBOntology.ICON)
    WebResource getIcon();

    void setIcon(WebResource icon);

    @rdf(MOBOntology.MARKERTAG)
    Set<Marker> getMarkerTag();

    void setMarkerTag(Set<Marker> markerTag);

    @rdf(MOBOntology.NAME)
    String getName();

    void setName(String name);

    @rdf(MOBOntology.RICHTEXTDESCRIPTION)
    String getRichTextDescription();

    void setRichTextDescription(String richTextDescription);

    // Note: the mob:score property is not used.  Rather, it serves as a parent
    //       for other, dynamically-defined properties.

    @rdf(MOBOntology.SENSITIVITY)
    SensitivityLevel getSensitivity();

    void setSensitivity(SensitivityLevel sensitivity);
}
