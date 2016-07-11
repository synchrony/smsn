package net.fortytwo.smsn.monitron.ontologies;

import net.fortytwo.rdfagents.RDFAgents;
import org.openrdf.model.IRI;

/**
 * @author Joshua Shinavier (http://fortytwo.net)
 */
public interface EventOntology {
    public static final String NAMESPACE = "http://purl.org/NET/c4dm/event.owl#";

    public static final IRI
            EVENT = RDFAgents.createIRI(NAMESPACE + "Event"),
            PLACE = RDFAgents.createIRI(NAMESPACE + "place"),
            SUB_EVENT = RDFAgents.createIRI(NAMESPACE + "sub_event"),
            TIME = RDFAgents.createIRI(NAMESPACE + "time");
}
