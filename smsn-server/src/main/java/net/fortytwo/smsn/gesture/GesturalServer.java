package net.fortytwo.smsn.gesture;

import com.illposed.osc.OSCBundle;
import com.illposed.osc.OSCListener;
import com.illposed.osc.OSCMessage;
import com.illposed.osc.OSCPortIn;
import net.fortytwo.smsn.p2p.osc.OscSender;
import net.fortytwo.smsn.p2p.osc.udp.UdpOscSender;
import net.fortytwo.smsn.rdf.Activities;
import net.fortytwo.smsn.rdf.RDFDataset;
import net.fortytwo.smsn.rdf.vocab.SmSnActivityOntology;
import org.apache.commons.text.StringEscapeUtils;
import org.eclipse.rdf4j.model.IRI;
import org.eclipse.rdf4j.model.ValueFactory;
import org.eclipse.rdf4j.model.impl.SimpleValueFactory;

import java.io.IOException;
import java.net.SocketException;
import java.net.UnknownHostException;
import java.util.List;
import java.util.function.Consumer;
import java.util.logging.Level;
import java.util.logging.Logger;

/**
 * Gestural server for recognizing handshakes, high-fives, and hand-off interactions.
 *
 * This server receives OSC (Open Sound Control) messages from wearable devices
 * and matches gesture events from multiple participants to recognize interactions.
 *
 * NOTE: The Dataset output was changed from rdfagents Dataset to RDFDataset in Dec 2024
 * during RDF4J migration. The rdfagents library used deprecated OpenRDF Sesame APIs.
 */
public class GesturalServer {
    private static final Logger logger = Logger.getLogger(GesturalServer.class.getName());

    private static final ValueFactory valueFactory = SimpleValueFactory.getInstance();

    private static final int DEFAULT_PORT = 42003;

    private final HandshakeMatcher handshakeMatcher;
    private final HandoffMatcher handoffMatcher;
    private final HighFiveMatcher highFiveMatcher;

    private final int port;

    private static final boolean doSpeak = false;

    private final OscSender notificationSender;

    public GesturalServer(final Consumer<RDFDataset> datasetHandler) {
        this(DEFAULT_PORT, datasetHandler);
    }

    public GesturalServer(final int port,
                          final Consumer<RDFDataset> datasetHandler) {
        this.port = port;

        // TODO: host and port are temporary; they should be configurable
        try {
            notificationSender = new UdpOscSender("localhost", 42003);
        } catch (UnknownHostException | SocketException e) {
            throw new IllegalStateException();
        }

        HandshakeMatcher.HandshakeHandler handshakeHandler = (left, right, timestamp) -> {
            notifyOfInteraction(SmSnActivityOntology.EXO_ACTIVITY_HANDSHAKE);

            RDFDataset d = Activities.datasetForHandshakeInteraction(timestamp, left.actor, right.actor);
            datasetHandler.accept(d);

            speakWithSystemCall(left.actor.getLocalName() + " shook hands with " + right.actor.getLocalName());
        };

        handshakeMatcher = new HandshakeMatcher(handshakeHandler);

        HandoffMatcher.HandoffHandler handoffHandler = (give, take, thingGiven, timestamp) -> {
            notifyOfInteraction(SmSnActivityOntology.EXO_ACTIVITY_HANDOFF);

            RDFDataset d = Activities.datasetForHandoffInteraction(timestamp, give.actor, take.actor, thingGiven);
            datasetHandler.accept(d);

            speakWithSystemCall(give.actor.getLocalName() + " gave \"" + thingGiven.getLocalName()
                    + "\" to " + take.actor.getLocalName());
        };

        handoffMatcher = new HandoffMatcher(handoffHandler);

        HighFiveMatcher.HighFiveHandler highFiveHandler = (left, right, time) -> {
            notifyOfInteraction(SmSnActivityOntology.EXO_ACTIVITY_HIGHFIVE);

            speakWithSystemCall(left.actor.getLocalName() + " high-fived " + right.actor.getLocalName());
        };

        highFiveMatcher = new HighFiveMatcher(highFiveHandler);
    }

    private void notifyOfInteraction(final String activityAddress) {
        OSCMessage m = new OSCMessage(activityAddress);
        OSCBundle bundle = new OSCBundle();
        bundle.addPacket(m);
        notificationSender.send(bundle);
    }

    private boolean badArgs(final List<Object> args,
                            final int expected,
                            final String address) {
        if (args.size() != expected) {
            logger.warning("expected " + expected + " arguments to " + address + "; got " + args.size()
                    + ". Ignoring event.");

            return true;
        } else {
            return false;
        }
    }

    private IRI getActor(final List<Object> args) {
        Object arg = args.get(0);
        if (!(arg instanceof String)) {
            logger.warning("actor argument must be a String");
            return null;
        }
        try {
            return valueFactory.createIRI((String) arg);
        } catch (IllegalArgumentException e) {
            logger.warning("actor not provided as a valid IRI: " + arg);
            return null;
        }
    }

    private long getRecognitionTimestamp(final List<Object> args) {
        // For now, use the current instant as the gestural timestamp,
        // disregarding all considerations of latency.
        // We ignore both the estimated moment of occurrence and the moment of recognition provided in the message.
        // TODO: this ignores the fact that UDP does not guarantee ordered delivery
        return System.currentTimeMillis();
    }

    public void start() throws SocketException {
        OSCListener handshakeListener = (date, oscMessage) -> {
            List<Object> args = oscMessage.getArguments();
            if (badArgs(args, 1, SmSnActivityOntology.EXO_ACTIVITY_HANDSHAKE)) {
                return;
            }

            IRI actor = getActor(args);
            if (null == actor) {
                return;
            }

            long timestamp = getRecognitionTimestamp(args);

            long now = System.currentTimeMillis();
            System.out.println("" + now + ": received half-handshake from " + actor);
            handshakeMatcher.receiveEvent(actor, timestamp, now);
        };

        // this serves as both the "give" and "take" half of the hand-off interaction
        OSCListener handoffListener = (date, oscMessage) -> {
            List<Object> args = oscMessage.getArguments();
            if (badArgs(args, 1, SmSnActivityOntology.EXO_ACTIVITY_HANDOFF)) {
                return;
            }

            IRI actor = getActor(args);
            if (null == actor) {
                return;
            }

            long timestamp = getRecognitionTimestamp(args);

            System.out.println("" + System.currentTimeMillis() + ": received handoff gesture from " + actor);
            handoffMatcher.receiveEvent(actor, timestamp);
        };

        // this is where the "giver" in a handoff interaction provides the item to give
        OSCListener giveListener = (date, oscMessage) -> {

            List<Object> args = oscMessage.getArguments();
            if (badArgs(args, 2, SmSnActivityOntology.EXO_ACTIVITY_GIVE)) {
                return;
            }

            IRI actor = valueFactory.createIRI((String) args.get(0));
            IRI thingGiven = valueFactory.createIRI((String) args.get(1));

            System.out.println(actor + " gave " + thingGiven);

            handoffMatcher.prepareForGive(actor, thingGiven, System.currentTimeMillis());
        };

        OSCListener highFiveListener = (date, oscMessage) -> {
            List<Object> args = oscMessage.getArguments();
            if (badArgs(args, 1, SmSnActivityOntology.EXO_ACTIVITY_HIGHFIVE)) {
                return;
            }

            IRI actor = getActor(args);
            if (null == actor) {
                return;
            }

            long timestamp = getRecognitionTimestamp(args);

            System.out.println("" + System.currentTimeMillis() + ": received high-five clap from " + actor);
            highFiveMatcher.receiveEvent(actor, timestamp);
        };

        OSCListener infoListener = (date, oscMessage) ->
                logger.info("info message via OSC: " + oscMessage.getArguments().get(0));

        OSCListener errorListener = (date, oscMessage) ->
                logger.warning("error message via OSC: " + oscMessage.getArguments().get(0));

        final OSCPortIn portIn = new OSCPortIn(port);
        portIn.addListener(SmSnActivityOntology.EXO_ACTIVITY_GIVE, giveListener);
        portIn.addListener(SmSnActivityOntology.EXO_ACTIVITY_HANDOFF, handoffListener);
        portIn.addListener(SmSnActivityOntology.EXO_ACTIVITY_HANDSHAKE, handshakeListener);
        portIn.addListener(SmSnActivityOntology.EXO_ACTIVITY_HIGHFIVE, highFiveListener);
        portIn.addListener("/exo/hand/info", infoListener);
        portIn.addListener("/exo/hand/error", errorListener);

        new Thread(() -> {
            try {
                // Loop indefinitely, first starting the OSC listener, then restarting it if it fails for any reason
                // This happens when it receives a badly-formatted message.
                while (true) {
                    if (!portIn.isListening()) {
                        logger.info("listening for /exo messages");
                        System.out.println("\tnot listening");
                        portIn.startListening();
                    }

                    Thread.sleep(500);
                }
            } catch (Throwable t) {
                logger.log(Level.SEVERE, "gestural server listener died with error", t);
            }
        }).start();
    }

    public static void main(final String[] args) throws Exception {
        Consumer<RDFDataset> h = dataset -> {
            // discard dataset
        };

        new GesturalServer(h).start();

        while (true) {
            Thread.sleep(10000);
        }
    }

    private void speakWithSystemCall(final String message) {
        System.out.println("SPEAKING: " + message);

        if (!doSpeak) {
            return;
        }

        Process p = null;
        try {
            // TODO: temporary for demo
            Runtime runtime = Runtime.getRuntime();
            p = runtime.exec("say \"" + StringEscapeUtils.escapeJava(message) + "\"");
        } catch (IOException e) {
            logger.log(Level.WARNING, "'say' command failed", e);
        }
        if (null != p) {
            int exitCode = 0;
            try {
                exitCode = p.waitFor();
            } catch (InterruptedException e) {
                logger.log(Level.SEVERE, "interrupted while waiting for 'say' command", e);
            }
            if (0 != exitCode) {
                logger.warning("'say' command failed with code " + exitCode);
            }
        }
    }
}
