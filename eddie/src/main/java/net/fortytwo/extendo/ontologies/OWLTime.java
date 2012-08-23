package net.fortytwo.extendo.ontologies;

import org.openrdf.model.URI;
import org.openrdf.model.impl.URIImpl;

/**
 * @author Joshua Shinavier (http://fortytwo.net)
 */
public interface OWLTime {
    public static final String NAMESPACE = "http://www.w3.org/2006/time#";

    public static final URI
            AFTER = new URIImpl(NAMESPACE + "after"),
            BEFORE = new URIImpl(NAMESPACE + "before"),
            INSIDE = new URIImpl(NAMESPACE + "inside"),
            IN_XSD_DATE_TIME = new URIImpl(NAMESPACE + "inXSDDateTime");
}
