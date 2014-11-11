package net.fortytwo.extendo.server.gesture;

import com.illposed.osc.OSCListener;
import com.illposed.osc.OSCMessage;
import com.illposed.osc.OSCPortIn;

import java.net.SocketException;
import java.util.Date;
import java.util.List;
import java.util.logging.Logger;

/**
 * @author Joshua Shinavier (http://fortytwo.net)
 */
public class GesturalServer {
    private static final Logger logger = Logger.getLogger(GesturalServer.class.getName());

    private static final int PORT_IN = 42003;

    private final HandshakeMatcher handshakeMatcher;

    public GesturalServer() {
        HandshakeMatcher.HandshakeHandler handler = new HandshakeMatcher.HandshakeHandler() {
            @Override
            public void handle(HandshakeMatcher.Handshake left, HandshakeMatcher.Handshake right) {
                System.out.println("handshake between " + left.actor + " and " + right.actor + " at " + System.currentTimeMillis());
            }
        };

        handshakeMatcher = new HandshakeMatcher(handler);
    }

    public void start() throws SocketException {
        OSCListener gestureListener = new OSCListener() {
            @Override
            public void acceptMessage(Date date, OSCMessage oscMessage) {
                System.out.println("received gestural message");
                List<Object> args = oscMessage.getArguments();
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

                if (gesture.equals("handshake")) {
                    // for now, use the current instant as the gestural timestamp,
                    // disregarding all considerations of latency
                    // TODO: this ignores the fact that UDP does not guarantee ordered delivery
                    long timestamp = System.currentTimeMillis();

                    handshakeMatcher.receiveEvent(name, timestamp);
                }
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

        OSCPortIn portIn = new OSCPortIn(PORT_IN);
        // TODO: these messages should really be directed at /exo/server/gesture
        portIn.addListener("/exo/hand/gesture", gestureListener);
        portIn.addListener("/exo/hand/info", infoListener);
        portIn.addListener("/exo/hand/error", errorListener);
        logger.info("listening for /exo messages");
        portIn.startListening();
    }

    public static void main(final String[] args) throws Exception {
        new GesturalServer().start();
        while (true) {
            Thread.sleep(10000);
        }
    }
}
