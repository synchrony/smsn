package net.fortytwo.extendo.server.gesture;

import com.illposed.osc.OSCListener;
import com.illposed.osc.OSCMessage;
import com.illposed.osc.OSCPortIn;
import net.fortytwo.extendo.rdf.Activities;
import net.fortytwo.extendo.rdf.vocab.ExtendoActivityOntology;
import net.fortytwo.rdfagents.model.Dataset;
import net.fortytwo.ripple.StringUtils;
import org.openrdf.model.URI;
import org.openrdf.model.impl.URIImpl;

import java.io.IOException;
import java.net.SocketException;
import java.util.Date;
import java.util.List;
import java.util.logging.Level;
import java.util.logging.Logger;

/**
 * @author Joshua Shinavier (http://fortytwo.net)
 */
public class GesturalServer {
    private static final Logger logger = Logger.getLogger(GesturalServer.class.getName());

    private static final int DEFAULT_PORT = 42003;

    private final HandshakeMatcher handshakeMatcher;
    private final HandoffMatcher handoffMatcher;

    private final int port;

    private static final boolean doSpeak = false;

    public GesturalServer(final DatasetHandler datasetHandler) {
        this(DEFAULT_PORT, datasetHandler);
    }

    public GesturalServer(final int port,
                          final DatasetHandler datasetHandler) {
        this.port = port;

        HandshakeMatcher.HandshakeHandler handshakeHandler = new HandshakeMatcher.HandshakeHandler() {
            @Override
            public void handle(HandshakeMatcher.Handshake left, HandshakeMatcher.Handshake right, long timestamp) {
                speakWithSystemCall(left.actor.getLocalName() + " shook hands with " + right.actor.getLocalName());
                // + " at " + timestamp);

                Dataset d = Activities.datasetForHandshakeInteraction(timestamp, left.actor, right.actor);
                datasetHandler.handle(d);
            }
        };

        handshakeMatcher = new HandshakeMatcher(handshakeHandler);

        HandoffMatcher.HandoffHandler handoffHandler = new HandoffMatcher.HandoffHandler() {
            @Override
            public void handle(HandoffMatcher.Handoff give,
                               HandoffMatcher.Handoff take,
                               URI thingGiven,
                               long timestamp) {
                speakWithSystemCall(give.actor.getLocalName() + " gave \"" + thingGiven.getLocalName()
                        + "\" to " + take.actor.getLocalName());
                // + " at " + timestamp);

                Dataset d = Activities.datasetForHandoffInteraction(timestamp, give.actor, take.actor, thingGiven);
                datasetHandler.handle(d);
            }
        };

        handoffMatcher = new HandoffMatcher(handoffHandler);
    }

    private boolean badArgs(final List<Object> args,
                            final int expected,
                            final String address) {
        if (args.size() != expected) {
            logger.warning("expected " + expected + " arguments to " + address + "; got " + args.size()
                    + ". Ignoring event.");

            // TODO: temporary.  Beware of an apparent JavaOSC bug when sending certain combinations of arguments.
            //for (Object arg : args) {
            //    System.err.println("\targ: " + arg);
            //}
            return true;
        } else {
            return false;
        }
    }

    private URI getActor(final List<Object> args) {
        Object arg = args.get(0);
        if (!(arg instanceof String)) {
            logger.warning("actor argument must be a String");
            return null;
        }
        try {
            return new URIImpl((String) arg);
        } catch (IllegalArgumentException e) {
            logger.warning("actor not provided as a valid URI: " + arg);
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
        OSCListener handshakeListener = new OSCListener() {
            @Override
            public void acceptMessage(Date date, OSCMessage oscMessage) {
                List<Object> args = oscMessage.getArguments();
                if (badArgs(args, 1, ExtendoActivityOntology.EXO_ACTIVITY_HANDSHAKE)) {
                    return;
                }

                URI actor = getActor(args);
                if (null == actor) {
                    return;
                }

                long timestamp = getRecognitionTimestamp(args);

                System.out.println("received half-handshake gesture from " + actor);
                handshakeMatcher.receiveEvent(actor, timestamp);
            }
        };

        OSCListener handoffListener = new OSCListener() {
            @Override
            public void acceptMessage(Date date, OSCMessage oscMessage) {
                List<Object> args = oscMessage.getArguments();
                if (badArgs(args, 1, ExtendoActivityOntology.EXO_ACTIVITY_HANDOFF)) {
                    return;
                }

                URI actor = getActor(args);
                if (null == actor) {
                    return;
                }

                long timestamp = getRecognitionTimestamp(args);

                System.out.println("received handoff gesture from " + actor);
                handoffMatcher.receiveEvent(actor, timestamp);
            }
        };

        OSCListener giveListener = new OSCListener() {
            @Override
            public void acceptMessage(Date date, OSCMessage oscMessage) {

                //byte[] buffer = oscMessage.getByteArray();
                //System.out.println("received message of length " + buffer.length + ": " + new String(buffer));

                List<Object> args = oscMessage.getArguments();
                if (badArgs(args, 2, ExtendoActivityOntology.EXO_ACTIVITY_GIVE)) {
                    return;
                }

                URI actor = new URIImpl((String) args.get(0));
                URI thingGiven = new URIImpl((String) args.get(1));
                //String value = (String) args.get(2);

                System.out.println(actor + " gave " + thingGiven);
                //System.out.println(actor + " gave " + thingGiven + " (" + value + ")");

                handoffMatcher.prepareForGive(actor, thingGiven, System.currentTimeMillis());
            }
        };

        OSCListener infoListener = new OSCListener() {
            @Override
            public void acceptMessage(Date date, OSCMessage oscMessage) {
                logger.info("info message via OSC: " + oscMessage.getArguments().get(0));
            }
        };

        OSCListener errorListener = new OSCListener() {
            @Override
            public void acceptMessage(Date date, OSCMessage oscMessage) {
                logger.warning("error message via OSC: " + oscMessage.getArguments().get(0));
            }
        };

        OSCPortIn portIn = new OSCPortIn(port);
        portIn.addListener(ExtendoActivityOntology.EXO_ACTIVITY_GIVE, giveListener);
        portIn.addListener(ExtendoActivityOntology.EXO_ACTIVITY_HANDOFF, handoffListener);
        portIn.addListener(ExtendoActivityOntology.EXO_ACTIVITY_HANDSHAKE, handshakeListener);
        portIn.addListener("/exo/hand/info", infoListener);
        portIn.addListener("/exo/hand/error", errorListener);
        logger.info("listening for /exo messages");
        portIn.startListening();
    }

    public static void main(final String[] args) throws Exception {
        DatasetHandler h = new DatasetHandler() {
            @Override
            public void handle(Dataset dataset) {
                // discard dataset
            }
        };

        new GesturalServer(h).start();
        while (true) {
            Thread.sleep(10000);
        }
    }

    // TODO: temporary for demo
    private final Runtime runtime = Runtime.getRuntime();

    private void speakWithSystemCall(final String message) {
        System.out.println("SPEAKING: " + message);

        if (!doSpeak) {
            return;
        }

        Process p = null;
        try {
            p = runtime.exec("say \"" + StringUtils.escapeString(message) + "\"");
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

    public interface DatasetHandler {
        void handle(Dataset dataset);
    }
}
