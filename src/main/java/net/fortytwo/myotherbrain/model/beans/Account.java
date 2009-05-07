package net.fortytwo.myotherbrain.model.beans;

import net.fortytwo.myotherbrain.model.MOB;
import org.openrdf.concepts.owl.Thing;
import org.openrdf.elmo.annotations.rdf;

import java.net.URI;

/**
 * Author: josh
 * Date: May 7, 2009
 * Time: 7:35:34 PM
 */
@rdf(MOB.NAMESPACE + "Account")
public interface Account extends Thing {
    @rdf(MOB.NAMESPACE + "privateGraph")
    Graph getPrivateGraph();

    void setPrivateGraph(Graph privateGraph);

    @rdf(MOB.NAMESPACE + "passwordSha1Sum")
    String getPasswordSha1Sum();

    void setPasswordSha1Sum(String passwordSha1Sum);


    @rdf(MOB.NAMESPACE + "contactEmailAddress")
    URI getContactEmailAddress();

    void setContactEmailAddress(URI contactEmailAddress);

    @rdf(MOB.NAMESPACE + "userName")
    String getUserName();

    void setUserName(String userName);
}
