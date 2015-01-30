package net.fortytwo.extendo.rdf.vocab;

import org.openrdf.model.URI;
import org.openrdf.model.impl.URIImpl;

/**
 * The Extendo gesture/activity ontology as a collection of terms
 *
 * @author Joshua Shinavier (http://fortytwo.net).
 */
public interface ExtendoActivityOntology {
    public static final String NAMESPACE = "http://fortytwo.net/2015/extendo/activity#";

    // OSC addresses shared between modules.  These are the OSC equivalents of the ontology classes.
    public static final String
            EXO_ACTIVITY_ATTENTION = "/exo/activity/attention",
            EXO_ACTIVITY_GIVE = "/exo/activity/give",
            EXO_ACTIVITY_HANDOFF = "/exo/activity/handoff",
            EXO_ACTIVITY_HANDSHAKE = "/exo/activity/handshake",
            EXO_ACTIVITY_POINT = "/exo/activity/point";

    // classes
    public static final URI
            Activity = new URIImpl(NAMESPACE + "Activity"),
            Attention = new URIImpl(NAMESPACE + "Attention"),
            BatonGesture = new URIImpl(NAMESPACE + "BatonGesture"),
            Handoff = new URIImpl(NAMESPACE + "Handoff"),
            Handshake = new URIImpl(NAMESPACE + "Handshake"),
            Interaction = new URIImpl(NAMESPACE + "Interaction"),
            Gesture = new URIImpl(NAMESPACE + "Gesture"),
            Point = new URIImpl(NAMESPACE + "Point");

    // properties
    public static final URI
            actor = new URIImpl(NAMESPACE + "actor"),
            focusOfAttention = new URIImpl(NAMESPACE + "focusOfAttention"),
            giver = new URIImpl(NAMESPACE + "giver"),
            recognitionTime = new URIImpl(NAMESPACE + "recognitionTime"),
            taker = new URIImpl(NAMESPACE + "taker"),
            thingGiven = new URIImpl(NAMESPACE + "thingGiven"),
            thingIndicated = new URIImpl(NAMESPACE + "thingIndicated");
}
