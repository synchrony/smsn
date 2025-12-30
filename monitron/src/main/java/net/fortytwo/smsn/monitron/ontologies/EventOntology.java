package net.fortytwo.smsn.monitron.ontologies;

import net.fortytwo.smsn.rdf.RDF4JUtil;
import org.eclipse.rdf4j.model.IRI;

public interface EventOntology {
    String NAMESPACE = "http://purl.org/NET/c4dm/event.owl#";

    IRI
            EVENT = RDF4JUtil.createIRI(NAMESPACE + "Event"),
            PLACE = RDF4JUtil.createIRI(NAMESPACE + "place"),
            SUB_EVENT = RDF4JUtil.createIRI(NAMESPACE + "sub_event"),
            TIME = RDF4JUtil.createIRI(NAMESPACE + "time");
}
