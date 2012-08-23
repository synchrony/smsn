package net.fortytwo.extendo.ontologies;

import org.openrdf.model.URI;
import org.openrdf.model.impl.URIImpl;

/**
 * @author Joshua Shinavier (http://fortytwo.net)
 */
public interface EventOntology {
    public static final String NAMESPACE = "http://purl.org/NET/c4dm/event.owl#";

    public static final URI
            EVENT = new URIImpl(NAMESPACE + "Event"),
            PLACE = new URIImpl(NAMESPACE + "place"),
            SUB_EVENT = new URIImpl(NAMESPACE + "sub_event"),
            TIME = new URIImpl(NAMESPACE + "time");
}
