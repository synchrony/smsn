package net.fortytwo.smsn.rdf.vocab;

import net.fortytwo.rdfagents.RDFAgents;
import org.openrdf.model.IRI;

/**
 * A selection of terms from the DBpedia Ontology
 */
public class DBpediaOntology {
    private static final String NAMESPACE = "http://dbpedia.org/ontology/";

    // properties
    public static final IRI
            owner = RDFAgents.createIRI(NAMESPACE + "owner");
}
