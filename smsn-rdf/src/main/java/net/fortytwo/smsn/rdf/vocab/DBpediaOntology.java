package net.fortytwo.smsn.rdf.vocab;

import org.openrdf.model.URI;
import org.openrdf.model.impl.URIImpl;

/**
 * A selection of terms from the DBpedia Ontology
 *
 * @author Joshua Shinavier (http://fortytwo.net)
 */
public class DBpediaOntology {
    public static final String NAMESPACE = "http://dbpedia.org/ontology/";

    // properties
    public static final URI
            owner = new URIImpl(NAMESPACE + "owner");
}
