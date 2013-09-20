package net.fortytwo.extendo.brain.rdf.vocab;

import org.openrdf.model.URI;
import org.openrdf.model.impl.URIImpl;

/**
 * @author Joshua Shinavier (http://fortytwo.net).
 */
public interface DCTerms {
    public static final String NAMESPACE = "http://purl.org/dc/terms/";

    public static final URI
            CREATED = new URIImpl(NAMESPACE + "created"),
            TITLE = new URIImpl(NAMESPACE + "title");
    // TODO: other terms
}
