package net.fortytwo.smsn.rdf.vocab;

import net.fortytwo.rdfagents.RDFAgents;
import org.openrdf.model.IRI;

/**
 * The Event Ontology as a collection of terms
 * This class was created on 2013-09-19 from Event Ontology version published 25th October 2007
 *
 * @author Joshua Shinavier (http://fortytwo.net).
 */
public interface Event {
    public static final String NAMESPACE = "http://purl.org/NET/c4dm/event.owl#";

    // classes
    public static final IRI
            Event = RDFAgents.createIRI(NAMESPACE + "Event"),
            Factor = RDFAgents.createIRI(NAMESPACE + "Factor"),
            Product = RDFAgents.createIRI(NAMESPACE + "Product");

    // properties
    public static final IRI
            agent = RDFAgents.createIRI(NAMESPACE + "agent"),
            agent_in = RDFAgents.createIRI(NAMESPACE + "agent_in"),
            factor = RDFAgents.createIRI(NAMESPACE + "factor"),
            factor_of = RDFAgents.createIRI(NAMESPACE + "factor_of"),
            hasAgent = RDFAgents.createIRI(NAMESPACE + "hasAgent"),
            hasFactor = RDFAgents.createIRI(NAMESPACE + "hasFactor"),
            hasLiteralFactor = RDFAgents.createIRI(NAMESPACE + "hasLiteralFactor"),
            hasProduct = RDFAgents.createIRI(NAMESPACE + "hasProduct"),
            hasSubEvent = RDFAgents.createIRI(NAMESPACE + "hasSubEvent"),
            isAgentIn = RDFAgents.createIRI(NAMESPACE + "isAgentIn"),
            isFactorOf = RDFAgents.createIRI(NAMESPACE + "isFactorOf"),
            literal_factor = RDFAgents.createIRI(NAMESPACE + "literal_factor"),
            place = RDFAgents.createIRI(NAMESPACE + "place"),
            producedIn = RDFAgents.createIRI(NAMESPACE + "producedIn"),
            produced_in = RDFAgents.createIRI(NAMESPACE + "produced_in"),
            product = RDFAgents.createIRI(NAMESPACE + "product"),
            sub_event = RDFAgents.createIRI(NAMESPACE + "sub_event"),
            time = RDFAgents.createIRI(NAMESPACE + "time");
}
