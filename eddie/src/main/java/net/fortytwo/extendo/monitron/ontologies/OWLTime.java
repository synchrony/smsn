package net.fortytwo.extendo.monitron.ontologies;

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
            HAS_BEGINNING = new URIImpl(NAMESPACE + "hasBeginning"),
            HAS_END = new URIImpl(NAMESPACE + "hasEnd"),
            INSIDE = new URIImpl(NAMESPACE + "inside"),
            INSTANT = new URIImpl(NAMESPACE + "Instant"),
            INTERVAL = new URIImpl(NAMESPACE + "Interval"),
            IN_XSD_DATE_TIME = new URIImpl(NAMESPACE + "inXSDDateTime");
}
