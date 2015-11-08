package net.fortytwo.smsn.monitron.ontologies;

import org.openrdf.model.URI;
import org.openrdf.model.impl.URIImpl;

/**
 * @author Joshua Shinavier (http://fortytwo.net)
 */
public interface OMOntology {
    public static final String NAMESPACE = "http://def.seegrid.csiro.au/ontology/om/om-lite#";

    public static final URI
            FEATURE_OF_INTEREST = new URIImpl(NAMESPACE + "featureOfInterest"),
            MEASUREMENT = new URIImpl(NAMESPACE + "Measurement"),
            OBSERVATION = new URIImpl(NAMESPACE + "Observation"),
            OBSERVED_PROPERTY = new URIImpl(NAMESPACE + "observedProperty"),
            PROCEDURE = new URIImpl(NAMESPACE + "procedure"),
            RESULT = new URIImpl(NAMESPACE + "result"),
            SIMPLE_MEASURE = new URIImpl(NAMESPACE + "SimpleMeasure"),
            PHENOMENON_TIME = new URIImpl(NAMESPACE + "phenomenonTime"),
            UOM = new URIImpl(NAMESPACE + "uom"),
            AMOUNT = new URIImpl(NAMESPACE + "amount");
}
