package net.fortytwo.smsn.rdf.vocab;

import net.fortytwo.smsn.rdf.RDF4JUtil;
import org.eclipse.rdf4j.model.IRI;

/**
 * A selection of terms from the DBpedia Ontology
 */
public class DBpediaOntology {
    private static final String NAMESPACE = "http://dbpedia.org/ontology/";

    // properties
    public static final IRI
            owner = RDF4JUtil.createIRI(NAMESPACE + "owner");
}
