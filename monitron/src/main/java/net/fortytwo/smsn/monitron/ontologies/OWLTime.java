package net.fortytwo.smsn.monitron.ontologies;

import net.fortytwo.rdfagents.RDFAgents;
import org.openrdf.model.IRI;

public interface OWLTime {
    String NAMESPACE = "http://www.w3.org/2006/time#";

    IRI
            AFTER = RDFAgents.createIRI(NAMESPACE + "after"),
            BEFORE = RDFAgents.createIRI(NAMESPACE + "before"),
            HAS_BEGINNING = RDFAgents.createIRI(NAMESPACE + "hasBeginning"),
            HAS_END = RDFAgents.createIRI(NAMESPACE + "hasEnd"),
            INSIDE = RDFAgents.createIRI(NAMESPACE + "inside"),
            INSTANT = RDFAgents.createIRI(NAMESPACE + "Instant"),
            INTERVAL = RDFAgents.createIRI(NAMESPACE + "Interval"),
            IN_XSD_DATE_TIME = RDFAgents.createIRI(NAMESPACE + "inXSDDateTime");
}
