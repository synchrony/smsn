package net.fortytwo.myotherbrain.update;

import java.net.URI;


/**
 * Author: josh
 * Date: Jul 1, 2009
 * Time: 10:06:22 PM
 */
public class NoSuchItemException extends UpdateException {
    public NoSuchItemException(final URI subject,
                               final Class cl) {
        // TODO: use the URI of the class from the MOB ontology, instead of the Java class name
        super("item of class " + cl + " with URI <" + subject + "> cannot be found");
    }
}
