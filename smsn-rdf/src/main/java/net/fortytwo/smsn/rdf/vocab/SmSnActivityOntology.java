package net.fortytwo.smsn.rdf.vocab;

import net.fortytwo.rdfagents.RDFAgents;
import org.openrdf.model.IRI;

/**
 * The Semantic Synchrony gesture/activity ontology as a collection of terms
 *
 * @author Joshua Shinavier (http://fortytwo.net).
 */
public interface SmSnActivityOntology {
    public static final String NAMESPACE = "http://fortytwo.net/2015/smsn/activity#";

    // OSC addresses shared between modules.  These are the OSC equivalents of the ontology classes.
    public static final String
            EXO_ACTIVITY_ATTENTION = "/smsn/activity/attention",
            EXO_ACTIVITY_GIVE = "/smsn/activity/give",
            EXO_ACTIVITY_HANDOFF = "/smsn/activity/handoff",
            EXO_ACTIVITY_HANDSHAKE = "/smsn/activity/handshake",
            EXO_ACTIVITY_HIGHFIVE = "/smsn/activity/highfive",
            EXO_ACTIVITY_POINT = "/smsn/activity/point";

    // classes
    public static final IRI
            Activity = RDFAgents.createIRI(NAMESPACE + "Activity"),
            Attention = RDFAgents.createIRI(NAMESPACE + "Attention"),
            BatonGesture = RDFAgents.createIRI(NAMESPACE + "BatonGesture"),
            Handoff = RDFAgents.createIRI(NAMESPACE + "Handoff"),
            Handshake = RDFAgents.createIRI(NAMESPACE + "Handshake"),
            HandshakePulse = RDFAgents.createIRI(NAMESPACE + "HandshakePulse"),
            HighFive = RDFAgents.createIRI(NAMESPACE + "HighFive"),
            Interaction = RDFAgents.createIRI(NAMESPACE + "Interaction"),
            Gesture = RDFAgents.createIRI(NAMESPACE + "Gesture"),
            Motion = RDFAgents.createIRI(NAMESPACE + "Motion"),
            Point = RDFAgents.createIRI(NAMESPACE + "Point");

    // properties
    public static final IRI
            actor = RDFAgents.createIRI(NAMESPACE + "actor"),
            focusOfAttention = RDFAgents.createIRI(NAMESPACE + "focusOfAttention"),
            giver = RDFAgents.createIRI(NAMESPACE + "giver"),
            recognitionTime = RDFAgents.createIRI(NAMESPACE + "recognitionTime"),
            taker = RDFAgents.createIRI(NAMESPACE + "taker"),
            thingGiven = RDFAgents.createIRI(NAMESPACE + "thingGiven"),
            referent = RDFAgents.createIRI(NAMESPACE + "referent");
}
