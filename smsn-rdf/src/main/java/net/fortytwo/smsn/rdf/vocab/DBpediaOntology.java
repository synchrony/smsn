package net.fortytwo.smsn.rdf.vocab;

import net.fortytwo.rdfagents.RDFAgents;
import org.openrdf.model.IRI;

/**
 * A selection of terms from the DBpedia Ontology
 *
 * @author Joshua Shinavier (http://fortytwo.net)
 */
public class DBpediaOntology {
    public static final String NAMESPACE = "http://dbpedia.org/ontology/";

    // properties
    public static final IRI
            owner = RDFAgents.createIRI(NAMESPACE + "owner");
}
