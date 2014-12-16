package net.fortytwo.extendo.server.gesture;

import com.illposed.osc.OSCListener;
import com.illposed.osc.OSCMessage;
import com.illposed.osc.OSCPortIn;
import net.fortytwo.extendo.Extendo;
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

    public GesturalServer() {
        this(DEFAULT_PORT);
    }

    public GesturalServer(final int port) {
        this.port = port;

        HandshakeMatcher.HandshakeHandler handshakeHandler = new HandshakeMatcher.HandshakeHandler() {
            @Override
            public void handle(HandshakeMatcher.Handshake left, HandshakeMatcher.Handshake right, long timestamp) {
                speakWithSystemCall(left.actor + " shook hands with " + right.actor);// + " at " + timestamp);
            }
        };

        handshakeMatcher = new HandshakeMatcher(handshakeHandler);

        HandoffMatcher.HandoffHandler handoffHandler = new HandoffMatcher.HandoffHandler() {
            @Override
            public void handle(HandoffMatcher.Handoff give, HandoffMatcher.Handoff take, String thingGiven, long timestamp) {
                speakWithSystemCall(give.actor + " gave \"" + thingGiven + "\" to " + take.actor);// + " at " + timestamp);
            }
        };

        handoffMatcher = new HandoffMatcher(handoffHandler);
    }

    public void start() throws SocketException {
        OSCListener gestureListener = new OSCListener() {
            @Override
            public void acceptMessage(Date date, OSCMessage oscMessage) {
                List<Object> args = oscMessage.getArguments();
                /* TODO: restore this. These are the proper gestural arguments, but ExoHand on Arduino Nano is
                   too resource-constrained at the moment to support them
                if (4 != args.size()) {
                    logger.warning("gesture event has wrong # of args ("
                            + args.size() + ", expected 4). Ignoring event.");
                    return;
                }
                if (!(args.get(0) instanceof String)) {
                    logger.warning("expected first argument in gestural message to be a string-valued actor name." +
                            " Got: " + args.get(0) + ". Ignoring event.");
                    return;
                }
                if (!(args.get(1) instanceof String)) {
                    logger.warning("expected second argument in gestural message to be a string-valued gesture name." +
                            " Got: " + args.get(0) + ". Ignoring event.");
                    return;
                }

                String name = (String) args.get(0);
                String gesture = (String) args.get(1);
                */

                if (1 != args.size() || !(args.get(0) instanceof String)) {
                    logger.warning("gesture event has unexpected argument format..." +
                            " expecting 'hack' format with only one, string-valued argument (actor:gesturename)");
                    return;
                }

                String s = (String) args.get(0);
                int i = s.indexOf(':');
                if (i < 0) {
                    logger.warning("expected (actor:gesturename) format for the 'hack' argument");
                    return;
                }

                String actorName = s.substring(0, i);
                String gestureName = s.substring(i + 1);

                System.out.println("received " + gestureName + " gesture from " + actorName);

                // for now, use the current instant as the gestural timestamp,
                // disregarding all considerations of latency
                // TODO: this ignores the fact that UDP does not guarantee ordered delivery
                long timestamp = System.currentTimeMillis();

                if (gestureName.equals("handshake")) {
                    handshakeMatcher.receiveEvent(actorName, timestamp);
                } else if (gestureName.equals("handoff")) {
                    handoffMatcher.receiveEvent(actorName, timestamp);
                }
            }
        };
        OSCListener giveListener = new OSCListener() {
            @Override
            public void acceptMessage(Date date, OSCMessage oscMessage) {
                List<Object> args = oscMessage.getArguments();
                if (2 != args.size()) {
                    logger.warning("expected two arguments to " + Extendo.EXO_GESTURE_GIVE);
                    return;
                }

                String agentUri = (String) args.get(0);
                // TODO: passing the name instead of the URI is temporary
                String nameOfThingGiven = (String) args.get(1);
                System.out.println(agentUri + " gave " + nameOfThingGiven);

                URI sesameUri = new URIImpl(agentUri);
                handoffMatcher.prepareForGive(sesameUri, nameOfThingGiven, System.currentTimeMillis());
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
        // TODO: these messages should really be directed at /exo/server/gesture
        portIn.addListener("/exo/hand/gesture", gestureListener);
        portIn.addListener("/exo/hand/info", infoListener);
        portIn.addListener("/exo/hand/error", errorListener);
        portIn.addListener(Extendo.EXO_GESTURE_GIVE, giveListener);
        logger.info("listening for /exo messages");
        portIn.startListening();
    }

    public static void main(final String[] args) throws Exception {

        new GesturalServer().start();
        while (true) {
            Thread.sleep(10000);
        }
    }

    // TODO: temporary for demo
    private final Runtime runtime = Runtime.getRuntime();
    private void speakWithSystemCall(final String message) {
        System.out.println("SPEAKING: " + message);
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
}
