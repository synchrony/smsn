package net.fortytwo.smsn.monitron.ontologies;

import net.fortytwo.smsn.rdf.RDF4JUtil;
import org.eclipse.rdf4j.model.IRI;

public interface OWLTime {
    String NAMESPACE = "http://www.w3.org/2006/time#";

    IRI
            AFTER = RDF4JUtil.createIRI(NAMESPACE + "after"),
            BEFORE = RDF4JUtil.createIRI(NAMESPACE + "before"),
            HAS_BEGINNING = RDF4JUtil.createIRI(NAMESPACE + "hasBeginning"),
            HAS_END = RDF4JUtil.createIRI(NAMESPACE + "hasEnd"),
            INSIDE = RDF4JUtil.createIRI(NAMESPACE + "inside"),
            INSTANT = RDF4JUtil.createIRI(NAMESPACE + "Instant"),
            INTERVAL = RDF4JUtil.createIRI(NAMESPACE + "Interval"),
            IN_XSD_DATE_TIME = RDF4JUtil.createIRI(NAMESPACE + "inXSDDateTime");
}
