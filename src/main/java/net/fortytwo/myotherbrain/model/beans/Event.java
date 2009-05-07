package net.fortytwo.myotherbrain.model.beans;

import org.openrdf.elmo.annotations.rdf;
import net.fortytwo.myotherbrain.model.MOB;

import javax.xml.datatype.XMLGregorianCalendar;

/**
 * Author: josh
 * Date: May 7, 2009
 * Time: 7:53:04 PM
 */
@rdf(MOB.NAMESPACE + "Event")
public interface Event extends FirstClassItem {
    @rdf(MOB.NAMESPACE + "placeStamp")
    String getPlaceStamp();

    void setPlaceStamp(String placeStamp);

    @rdf(MOB.NAMESPACE + "timeStamp")
    XMLGregorianCalendar getTimeStamp();

    void setTimeStamp(XMLGregorianCalendar timeStamp);
}
