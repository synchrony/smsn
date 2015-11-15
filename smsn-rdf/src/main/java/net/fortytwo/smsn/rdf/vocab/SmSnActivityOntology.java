package net.fortytwo.smsn.rdf.vocab;

import org.openrdf.model.URI;
import org.openrdf.model.impl.URIImpl;

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
    public static final URI
            Activity = new URIImpl(NAMESPACE + "Activity"),
            Attention = new URIImpl(NAMESPACE + "Attention"),
            BatonGesture = new URIImpl(NAMESPACE + "BatonGesture"),
            Handoff = new URIImpl(NAMESPACE + "Handoff"),
            Handshake = new URIImpl(NAMESPACE + "Handshake"),
            HandshakePulse = new URIImpl(NAMESPACE + "HandshakePulse"),
            HighFive = new URIImpl(NAMESPACE + "HighFive"),
            Interaction = new URIImpl(NAMESPACE + "Interaction"),
            Gesture = new URIImpl(NAMESPACE + "Gesture"),
            Motion = new URIImpl(NAMESPACE + "Motion"),
            Point = new URIImpl(NAMESPACE + "Point");

    // properties
    public static final URI
            actor = new URIImpl(NAMESPACE + "actor"),
            focusOfAttention = new URIImpl(NAMESPACE + "focusOfAttention"),
            giver = new URIImpl(NAMESPACE + "giver"),
            recognitionTime = new URIImpl(NAMESPACE + "recognitionTime"),
            taker = new URIImpl(NAMESPACE + "taker"),
            thingGiven = new URIImpl(NAMESPACE + "thingGiven"),
            referent = new URIImpl(NAMESPACE + "referent");
}
