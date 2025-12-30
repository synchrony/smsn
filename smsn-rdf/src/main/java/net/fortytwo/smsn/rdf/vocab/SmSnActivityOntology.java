package net.fortytwo.smsn.rdf.vocab;

import net.fortytwo.smsn.rdf.RDF4JUtil;
import org.eclipse.rdf4j.model.IRI;

/**
 * The Semantic Synchrony gesture/activity ontology as a collection of terms
 */
public interface SmSnActivityOntology {
    String NAMESPACE = "http://fortytwo.net/2015/smsn/activity#";

    // OSC addresses shared between modules.  These are the OSC equivalents of the ontology classes.
    String
            EXO_ACTIVITY_ATTENTION = "/smsn/activity/attention",
            EXO_ACTIVITY_GIVE = "/smsn/activity/give",
            EXO_ACTIVITY_HANDOFF = "/smsn/activity/handoff",
            EXO_ACTIVITY_HANDSHAKE = "/smsn/activity/handshake",
            EXO_ACTIVITY_HIGHFIVE = "/smsn/activity/highfive",
            EXO_ACTIVITY_POINT = "/smsn/activity/point";

    // classes
    IRI
            Activity = RDF4JUtil.createIRI(NAMESPACE + "Activity"),
            Attention = RDF4JUtil.createIRI(NAMESPACE + "Attention"),
            BatonGesture = RDF4JUtil.createIRI(NAMESPACE + "BatonGesture"),
            Handoff = RDF4JUtil.createIRI(NAMESPACE + "Handoff"),
            Handshake = RDF4JUtil.createIRI(NAMESPACE + "Handshake"),
            HandshakePulse = RDF4JUtil.createIRI(NAMESPACE + "HandshakePulse"),
            HighFive = RDF4JUtil.createIRI(NAMESPACE + "HighFive"),
            Interaction = RDF4JUtil.createIRI(NAMESPACE + "Interaction"),
            Gesture = RDF4JUtil.createIRI(NAMESPACE + "Gesture"),
            Motion = RDF4JUtil.createIRI(NAMESPACE + "Motion"),
            Point = RDF4JUtil.createIRI(NAMESPACE + "Point");

    // properties
    IRI
            actor = RDF4JUtil.createIRI(NAMESPACE + "actor"),
            focusOfAttention = RDF4JUtil.createIRI(NAMESPACE + "focusOfAttention"),
            giver = RDF4JUtil.createIRI(NAMESPACE + "giver"),
            recognitionTime = RDF4JUtil.createIRI(NAMESPACE + "recognitionTime"),
            taker = RDF4JUtil.createIRI(NAMESPACE + "taker"),
            thingGiven = RDF4JUtil.createIRI(NAMESPACE + "thingGiven"),
            referent = RDF4JUtil.createIRI(NAMESPACE + "referent");
}
