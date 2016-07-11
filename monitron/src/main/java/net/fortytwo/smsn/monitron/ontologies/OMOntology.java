package net.fortytwo.smsn.monitron.ontologies;

import net.fortytwo.rdfagents.RDFAgents;
import org.openrdf.model.IRI;

/**
 * @author Joshua Shinavier (http://fortytwo.net)
 */
public interface OMOntology {
    public static final String NAMESPACE = "http://def.seegrid.csiro.au/ontology/om/om-lite#";

    public static final IRI
            FEATURE_OF_INTEREST = RDFAgents.createIRI(NAMESPACE + "featureOfInterest"),
            MEASUREMENT = RDFAgents.createIRI(NAMESPACE + "Measurement"),
            OBSERVATION = RDFAgents.createIRI(NAMESPACE + "Observation"),
            OBSERVED_PROPERTY = RDFAgents.createIRI(NAMESPACE + "observedProperty"),
            PROCEDURE = RDFAgents.createIRI(NAMESPACE + "procedure"),
            RESULT = RDFAgents.createIRI(NAMESPACE + "result"),
            SIMPLE_MEASURE = RDFAgents.createIRI(NAMESPACE + "SimpleMeasure"),
            PHENOMENON_TIME = RDFAgents.createIRI(NAMESPACE + "phenomenonTime"),
            UOM = RDFAgents.createIRI(NAMESPACE + "uom"),
            AMOUNT = RDFAgents.createIRI(NAMESPACE + "amount");
}
