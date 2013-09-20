package net.fortytwo.extendo.brain.rdf.vocab;

import org.openrdf.model.URI;
import org.openrdf.model.impl.URIImpl;

/**
 * @author Joshua Shinavier (http://fortytwo.net).
 */
// this class created on 2013-09-19 from Event Ontology version published 25th October 2007
public interface Event {
    public static final String NAMESPACE = "http://purl.org/NET/c4dm/event.owl#";

    // classes
    public static final URI
            Event = new URIImpl(NAMESPACE + "Event"),
            Factor = new URIImpl(NAMESPACE + "Factor"),
            Product = new URIImpl(NAMESPACE + "Product");

    // properties
    public static final URI
            agent = new URIImpl(NAMESPACE + "agent"),
            agent_in = new URIImpl(NAMESPACE + "agent_in"),
            factor = new URIImpl(NAMESPACE + "factor"),
            factor_of = new URIImpl(NAMESPACE + "factor_of"),
            hasAgent = new URIImpl(NAMESPACE + "hasAgent"),
            hasFactor = new URIImpl(NAMESPACE + "hasFactor"),
            hasLiteralFactor = new URIImpl(NAMESPACE + "hasLiteralFactor"),
            hasProduct = new URIImpl(NAMESPACE + "hasProduct"),
            hasSubEvent = new URIImpl(NAMESPACE + "hasSubEvent"),
            isAgentIn = new URIImpl(NAMESPACE + "isAgentIn"),
            isFactorOf = new URIImpl(NAMESPACE + "isFactorOf"),
            literal_factor = new URIImpl(NAMESPACE + "literal_factor"),
            place = new URIImpl(NAMESPACE + "place"),
            producedIn = new URIImpl(NAMESPACE + "producedIn"),
            produced_in = new URIImpl(NAMESPACE + "produced_in"),
            product = new URIImpl(NAMESPACE + "product"),
            sub_event = new URIImpl(NAMESPACE + "sub_event"),
            time = new URIImpl(NAMESPACE + "time");
}