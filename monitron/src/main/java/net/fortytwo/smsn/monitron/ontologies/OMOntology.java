package net.fortytwo.smsn.monitron.ontologies;

import net.fortytwo.smsn.rdf.RDF4JUtil;
import org.eclipse.rdf4j.model.IRI;

public interface OMOntology {
    String NAMESPACE = "http://def.seegrid.csiro.au/ontology/om/om-lite#";

    IRI
            FEATURE_OF_INTEREST = RDF4JUtil.createIRI(NAMESPACE + "featureOfInterest"),
            MEASUREMENT = RDF4JUtil.createIRI(NAMESPACE + "Measurement"),
            OBSERVATION = RDF4JUtil.createIRI(NAMESPACE + "Observation"),
            OBSERVED_PROPERTY = RDF4JUtil.createIRI(NAMESPACE + "observedProperty"),
            PROCEDURE = RDF4JUtil.createIRI(NAMESPACE + "procedure"),
            RESULT = RDF4JUtil.createIRI(NAMESPACE + "result"),
            SIMPLE_MEASURE = RDF4JUtil.createIRI(NAMESPACE + "SimpleMeasure"),
            PHENOMENON_TIME = RDF4JUtil.createIRI(NAMESPACE + "phenomenonTime"),
            UOM = RDF4JUtil.createIRI(NAMESPACE + "uom"),
            AMOUNT = RDF4JUtil.createIRI(NAMESPACE + "amount");
}
