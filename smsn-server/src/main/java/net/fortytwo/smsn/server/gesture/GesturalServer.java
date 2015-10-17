package net.fortytwo.smsn.server.gesture;

import com.illposed.osc.OSCBundle;
import com.illposed.osc.OSCListener;
import com.illposed.osc.OSCMessage;
import com.illposed.osc.OSCPortIn;
import net.fortytwo.smsn.p2p.osc.OscSender;
import net.fortytwo.smsn.p2p.osc.UdpOscSender;
import net.fortytwo.smsn.rdf.Activities;
import net.fortytwo.smsn.rdf.vocab.SmSnActivityOntology;
import net.fortytwo.rdfagents.model.Dataset;
import net.fortytwo.ripple.StringUtils;
import org.openrdf.model.URI;
import org.openrdf.model.impl.URIImpl;

import java.io.IOException;
import java.net.SocketException;
import java.net.UnknownHostException;
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
    private final HighFiveMatcher highFiveMatcher;

    private final int port;

    private static final boolean doSpeak = false;

    private final OscSender notificationSender;

    public GesturalServer(final DatasetHandler datasetHandler) {
        this(DEFAULT_PORT, datasetHandler);
    }

    public GesturalServer(final int port,
                          final DatasetHandler datasetHandler) {
        this.port = port;

        // TODO: host and port are temporary; they should be configurable
        try {
            notificationSender = new UdpOscSender("localhost", 42003);
        } catch (UnknownHostException e) {
            throw new IllegalStateException();
        } catch (SocketException e) {
            throw new IllegalStateException();
        }

        HandshakeMatcher.HandshakeHandler handshakeHandler = new HandshakeMatcher.HandshakeHandler() {
            @Override
            public void handle(HandshakeMatcher.HandshakeSequence left,
                               HandshakeMatcher.HandshakeSequence right, long timestamp) {
                notifyOfInteraction(SmSnActivityOntology.EXO_ACTIVITY_HANDSHAKE);

                Dataset d = Activities.datasetForHandshakeInteraction(timestamp, left.actor, right.actor);
                datasetHandler.handle(d);

                speakWithSystemCall(left.actor.getLocalName() + " shook hands with " + right.actor.getLocalName());
                // + " at " + timestamp);
            }
        };

        handshakeMatcher = new HandshakeMatcher(handshakeHandler);

        HandoffMatcher.HandoffHandler handoffHandler = new HandoffMatcher.HandoffHandler() {
            @Override
            public void handle(HandoffMatcher.Handoff give,
                               HandoffMatcher.Handoff take,
                               URI thingGiven,
                               long timestamp) {
                notifyOfInteraction(SmSnActivityOntology.EXO_ACTIVITY_HANDOFF);

                Dataset d = Activities.datasetForHandoffInteraction(timestamp, give.actor, take.actor, thingGiven);
                datasetHandler.handle(d);

                speakWithSystemCall(give.actor.getLocalName() + " gave \"" + thingGiven.getLocalName()
                        + "\" to " + take.actor.getLocalName());
                // + " at " + timestamp);
            }
        };

        handoffMatcher = new HandoffMatcher(handoffHandler);

        HighFiveMatcher.HighFiveHandler highFiveHandler = new HighFiveMatcher.HighFiveHandler() {
            @Override
            public void handle(HighFiveMatcher.Clap left, HighFiveMatcher.Clap right, long time) {
                notifyOfInteraction(SmSnActivityOntology.EXO_ACTIVITY_HIGHFIVE);

                speakWithSystemCall(left.actor.getLocalName() + " high-fived " + right.actor.getLocalName());
            }
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
                if (badArgs(args, 1, SmSnActivityOntology.EXO_ACTIVITY_HANDSHAKE)) {
                    return;
                }

                URI actor = getActor(args);
                if (null == actor) {
                    return;
                }

                long timestamp = getRecognitionTimestamp(args);

                long now = System.currentTimeMillis();
                System.out.println("" + now + ": received half-handshake from " + actor);
                handshakeMatcher.receiveEvent(actor, timestamp, now);
            }
        };

        // this serves as both the "give" and "take" half of the hand-off interaction
        OSCListener handoffListener = new OSCListener() {
            @Override
            public void acceptMessage(Date date, OSCMessage oscMessage) {
                List<Object> args = oscMessage.getArguments();
                if (badArgs(args, 1, SmSnActivityOntology.EXO_ACTIVITY_HANDOFF)) {
                    return;
                }

                URI actor = getActor(args);
                if (null == actor) {
                    return;
                }

                long timestamp = getRecognitionTimestamp(args);

                System.out.println("" + System.currentTimeMillis() + ": received handoff gesture from " + actor);
                handoffMatcher.receiveEvent(actor, timestamp);
            }
        };

        // this is where the "giver" in a handoff interaction provides the item to give
        OSCListener giveListener = new OSCListener() {
            @Override
            public void acceptMessage(Date date, OSCMessage oscMessage) {

                //byte[] buffer = oscMessage.getByteArray();
                //System.out.println("received message of length " + buffer.length + ": " + new String(buffer));

                List<Object> args = oscMessage.getArguments();
                if (badArgs(args, 2, SmSnActivityOntology.EXO_ACTIVITY_GIVE)) {
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

        OSCListener highFiveListener = new OSCListener() {
            @Override
            public void acceptMessage(Date date, OSCMessage oscMessage) {
                List<Object> args = oscMessage.getArguments();
                if (badArgs(args, 1, SmSnActivityOntology.EXO_ACTIVITY_HIGHFIVE)) {
                    return;
                }

                URI actor = getActor(args);
                if (null == actor) {
                    return;
                }

                long timestamp = getRecognitionTimestamp(args);

                System.out.println("" + System.currentTimeMillis() + ": received high-five clap from " + actor);
                highFiveMatcher.receiveEvent(actor, timestamp);
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

        final OSCPortIn portIn = new OSCPortIn(port);
        portIn.addListener(SmSnActivityOntology.EXO_ACTIVITY_GIVE, giveListener);
        portIn.addListener(SmSnActivityOntology.EXO_ACTIVITY_HANDOFF, handoffListener);
        portIn.addListener(SmSnActivityOntology.EXO_ACTIVITY_HANDSHAKE, handshakeListener);
        portIn.addListener(SmSnActivityOntology.EXO_ACTIVITY_HIGHFIVE, highFiveListener);
        portIn.addListener("/exo/hand/info", infoListener);
        portIn.addListener("/exo/hand/error", errorListener);

        new Thread(new Runnable() {
            @Override
            public void run() {
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
            }
        }).start();
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
