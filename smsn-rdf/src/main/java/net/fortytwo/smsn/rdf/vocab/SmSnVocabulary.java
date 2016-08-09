package net.fortytwo.smsn.rdf.vocab;

import net.fortytwo.rdfagents.RDFAgents;
import org.openrdf.model.IRI;

/**
 * The Personal Knowledge Base vocabulary provides additional terms
 * used in mapping an Extend-o-Brain knowledge base to RDF.
 * The majority of terms are drawn from other, widely-used ontologies.
 */
public interface SmSnVocabulary {
    public static final String NAMESPACE = "http://fortytwo.net/2015/smsn/pkb#";

    // classes
    public static final IRI
            TODO = RDFAgents.createIRI(NAMESPACE + "TODO"),      // temporary.  Find a more standard vocabulary term for TODOs
            WORDORPHRASE = RDFAgents.createIRI(NAMESPACE + "WordOrPhrase");  // temporary.  If it is possible to distinguish
                                                                     // between words and phrases, choose terms from
                                                                     // appropriate vocabularies
}
