package net.fortytwo.extendo.ontologies;

import org.openrdf.model.URI;
import org.openrdf.model.impl.URIImpl;

/**
 * @author Joshua Shinavier (http://fortytwo.net)
 */
public interface OMOntology {
    // TODO: this namespace is a placeholder
    public static final String NAMESPACE = "http://www.opengis.net/spec/OMXML/2.0/conf/";

    public static final URI
            FEATURE_OF_INTEREST = new URIImpl(NAMESPACE + "featureOfInterest"),
            OBSERVATION = new URIImpl(NAMESPACE + "Observation"),
            OBSERVATION_LOCATION = new URIImpl(NAMESPACE + "observationLocation"),
            OBSERVED_PROPERTY = new URIImpl(NAMESPACE + "observedProperty"),
            PROCEDURE = new URIImpl(NAMESPACE + "Procedure"),
            RESULT = new URIImpl(NAMESPACE + "result"),
            SAMPLING_TIME = new URIImpl(NAMESPACE + "samplingTime"),
            UOM = new URIImpl(NAMESPACE + "uom");



    /*

    measurementObservation
    truthObservation

     */
}
