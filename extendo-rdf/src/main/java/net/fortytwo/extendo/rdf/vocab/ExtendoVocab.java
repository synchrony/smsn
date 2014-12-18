package net.fortytwo.extendo.rdf.vocab;

import org.openrdf.model.URI;
import org.openrdf.model.impl.URIImpl;

/**
 * The Extendo Ontology as a collection of terms
 * This ontology provides additional terms used in mapping an Extend-o-Brain knowledge base to RDF.
 * The majority of terms are drawn from other, widely-used ontologies.
 *
 * @author Joshua Shinavier (http://fortytwo.net).
 */
public interface ExtendoVocab {
    public static final String NAMESPACE = "http://extendo.fortytwo.net/vocab/";

    // classes
    public static final URI
            TODO = new URIImpl(NAMESPACE + "TODO"),      // temporary.  Find a more standard vocabulary term for TODOs
            WORDORPHRASE = new URIImpl(NAMESPACE + "WordOrPhrase");  // temporary.  If it is possible to distinguish
                                                                     // between words and phrases, choose terms from
                                                                     // appropriate vocabularies
}
