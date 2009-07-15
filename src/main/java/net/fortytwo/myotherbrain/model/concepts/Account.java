package net.fortytwo.myotherbrain.model.concepts;

import net.fortytwo.myotherbrain.model.MOB;
import org.openrdf.concepts.owl.Thing;
import org.openrdf.elmo.annotations.rdf;

import java.net.URI;

/**
 * Author: josh
 * Date: May 7, 2009
 * Time: 7:35:34 PM
 */
@rdf(MOB.ACCOUNT)
public interface Account extends Thing {
    @rdf(MOB.CONTACTEMAILADDRESS)
    URI getContactEmailAddress();

    void setContactEmailAddress(URI contactEmailAddress);

    @rdf(MOB.PASSWORDSHA1SUM)
    String getPasswordSha1Sum();

    void setPasswordSha1Sum(String passwordSha1Sum);

    @rdf(MOB.PERSONALGRAPH)
    Graph getPersonalGraph();

    void setPersonalGraph(Graph personalGraph);

    @rdf(MOB.USERNAME)
    String getUserName();

    void setUserName(String userName);
}
