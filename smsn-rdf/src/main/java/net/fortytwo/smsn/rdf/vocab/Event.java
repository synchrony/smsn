package net.fortytwo.smsn.rdf.vocab;

import net.fortytwo.smsn.rdf.RDF4JUtil;
import org.eclipse.rdf4j.model.IRI;

/**
 * The Event Ontology as a collection of terms
 * This class was created on 2013-09-19 from Event Ontology version published 25th October 2007
 */
public interface Event {
    String NAMESPACE = "http://purl.org/NET/c4dm/event.owl#";

    // classes
    IRI
            Event = RDF4JUtil.createIRI(NAMESPACE + "Event"),
            Factor = RDF4JUtil.createIRI(NAMESPACE + "Factor"),
            Product = RDF4JUtil.createIRI(NAMESPACE + "Product");

    // properties
    IRI
            agent = RDF4JUtil.createIRI(NAMESPACE + "agent"),
            agent_in = RDF4JUtil.createIRI(NAMESPACE + "agent_in"),
            factor = RDF4JUtil.createIRI(NAMESPACE + "factor"),
            factor_of = RDF4JUtil.createIRI(NAMESPACE + "factor_of"),
            hasAgent = RDF4JUtil.createIRI(NAMESPACE + "hasAgent"),
            hasFactor = RDF4JUtil.createIRI(NAMESPACE + "hasFactor"),
            hasLiteralFactor = RDF4JUtil.createIRI(NAMESPACE + "hasLiteralFactor"),
            hasProduct = RDF4JUtil.createIRI(NAMESPACE + "hasProduct"),
            hasSubEvent = RDF4JUtil.createIRI(NAMESPACE + "hasSubEvent"),
            isAgentIn = RDF4JUtil.createIRI(NAMESPACE + "isAgentIn"),
            isFactorOf = RDF4JUtil.createIRI(NAMESPACE + "isFactorOf"),
            literal_factor = RDF4JUtil.createIRI(NAMESPACE + "literal_factor"),
            place = RDF4JUtil.createIRI(NAMESPACE + "place"),
            producedIn = RDF4JUtil.createIRI(NAMESPACE + "producedIn"),
            produced_in = RDF4JUtil.createIRI(NAMESPACE + "produced_in"),
            product = RDF4JUtil.createIRI(NAMESPACE + "product"),
            sub_event = RDF4JUtil.createIRI(NAMESPACE + "sub_event"),
            time = RDF4JUtil.createIRI(NAMESPACE + "time");
}
