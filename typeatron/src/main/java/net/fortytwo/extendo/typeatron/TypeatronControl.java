package net.fortytwo.extendo.typeatron;

import com.illposed.osc.OSCMessage;
import edu.rpi.twc.sesamestream.BindingSetHandler;
import edu.rpi.twc.sesamestream.QueryEngine;
import info.aduna.io.IOUtil;
import net.fortytwo.extendo.Extendo;
import net.fortytwo.extendo.brain.BrainModeClient;
import net.fortytwo.extendo.p2p.ExtendoAgent;
import net.fortytwo.extendo.p2p.SideEffects;
import net.fortytwo.extendo.p2p.osc.OscControl;
import net.fortytwo.extendo.p2p.osc.OscMessageHandler;
import net.fortytwo.extendo.p2p.osc.OscReceiver;
import net.fortytwo.extendo.rdf.Activities;
import net.fortytwo.extendo.typeatron.ripple.ExtendoRippleREPL;
import net.fortytwo.extendo.typeatron.ripple.RippleSession;
import net.fortytwo.extendo.typeatron.ripple.lib.music.TypeatronMusicControl;
import net.fortytwo.rdfagents.model.Dataset;
import net.fortytwo.ripple.RippleException;
import net.fortytwo.ripple.model.ModelConnection;
import org.openrdf.model.URI;
import org.openrdf.model.Value;
import org.openrdf.query.BindingSet;

import java.io.IOException;
import java.io.InputStream;
import java.io.PipedInputStream;
import java.io.PipedOutputStream;
import java.util.Date;
import java.util.List;
import java.util.logging.Level;
import java.util.logging.Logger;

/**
 * A controller for the Typeatron chorded keyer
 *
 * @author Joshua Shinavier (http://fortytwo.net)
 */
public class TypeatronControl extends OscControl {
    protected static final Logger logger = Logger.getLogger(TypeatronControl.class.getName());

    // for now, queries will not expire, and will not need to be renewed
    private static final int QUERY_TTL = 0;

    // fully specified, since PATH may or may not include /usr/bin
    private static final String EMACSCLIENT_BIN = "/usr/bin/emacsclient";

    // outbound addresses to Typeatron
    private static final String
            EXO_TT_LASER_FEEDBACK = "/exo/tt/laser/feedback",
            EXO_TT_LASER_OFF = "/exo/tt/laser/off",
            EXO_TT_LASER_ON = "/exo/tt/laser/on",
    //EXO_TT_LASER_TRIGGER = "/exo/tt/laser/trigger",
    EXO_TT_MORSE = "/exo/tt/morse",
            EXO_TT_OK = "/exo/tt/ok",
            EXO_TT_PHOTO_GET = "/exo/tt/photo/get",
            EXO_TT_PING = "/exo/tt/ping",
            EXO_TT_READY = "/exo/tt/ready",
            EXO_TT_VIBRO = "/exo/tt/vibro",
            EXO_TT_WARNING = "/exo/tt/warning";

    // inbound addresses from Typeatron
    private static final String
            EXO_TT_ERROR = "/exo/tt/error",  // note: also used as an outbound address
            EXO_TT_INFO = "/exo/tt/info",  // note: also used as an outbound address
            EXO_TT_KEYS = "/exo/tt/keys",
            EXO_TT_LASER_EVENT = "/exo/tt/laser/event",
            EXO_TT_PHOTO_DATA = "/exo/tt/photo/data",
            EXO_TT_PING_REPLY = "/exo/tt/ping/reply";

    public static final int
            VIBRATE_MANUAL_MS = 500;

    private final ExtendoAgent agent;
    private final SideEffects environment;

    private final BrainModeClientWrapper brainModeWrapper;
    private final RippleSession rippleSession;
    private final ExtendoRippleREPL rippleREPL;
    private final ChordedKeyer keyer;
    private final TypeatronMusicControl music;

    private URI thingPointedTo;

    public TypeatronControl(final OscReceiver oscReceiver,
                            final ExtendoAgent agent,
                            final SideEffects environment) throws DeviceInitializationException {
        super(oscReceiver);

        this.agent = agent;
        this.environment = environment;

        try {
            this.music = new TypeatronMusicControl();
        } catch (Exception e) {
            throw new DeviceInitializationException(e);
        }

        try {
            agent.getQueryEngine().addQuery(QUERY_TTL, Activities.QUERY_FOR_ATTENTION, new BindingSetHandler() {
                @Override
                public void handle(BindingSet b) {
                    Value actor = b.getValue("actor");
                    Value focus = b.getValue("focus");

                    if (keyer.getMode().equals(ChordedKeyer.Mode.Laser)) {
                        if (null == thingPointedTo) {
                            throw new IllegalStateException();
                        }

                        if (thingPointedTo.equals(focus)) {
                            sendLaserFeedbackMessage();
                        } else {
                            // TODO: temporary
                            logger.warning("got attentional feedback on item not pointed to" +
                                    " (OK IRL, but not expected in a demo)");
                        }
                    } else {
                        // TODO: temporary
                        logger.warning("got attentional feedback outside of Laser mode" +
                                " (OK IRL, but not expected in a demo)");
                    }

                    logger.info("notified of attention by " + actor + " to " + focus);
                }
            });
        } catch (IOException e) {
            throw new DeviceInitializationException("failed to create Typeatron's query subscription", e);
        } catch (QueryEngine.IncompatibleQueryException e) {
            throw new DeviceInitializationException("failed to create Typeatron's query subscription", e);
        } catch (QueryEngine.InvalidQueryException e) {
            throw new DeviceInitializationException("failed to create Typeatron's query subscription", e);
        }

        try {
            rippleSession = new RippleSession(agent, environment);

            ExtendoRippleREPL.REPLEventHandler eventHandler = new ExtendoRippleREPL.REPLEventHandler() {
                @Override
                public void beginCommand() {
                    sendReadyMessage();
                }

                @Override
                public void finishCommand() {
                    sendOkMessage();
                }
            };
            rippleREPL = new ExtendoRippleREPL(rippleSession, this, agent, environment, eventHandler);
        } catch (RippleException e) {
            throw new DeviceInitializationException(e);
        }

        try {
            ChordedKeyer.EventHandler handler = new ChordedKeyer.EventHandler() {
                @Override
                public void handleKeyPressed(int key) {
                    music.handleKeyPressed(key);
                }

                @Override
                public void handleKeyReleased(int key) {
                    music.handleKeyReleased(key);
                }

                @Override
                public void handleSymbol(ChordedKeyer.Mode mode, String symbol, ChordedKeyer.Modifier modifier) {
                    if (null != symbol) {
                        switch (mode) {
                            case Arrows:
                                // fall through
                            case TextEdit:
                                handleSymbolWithEmacs(symbol, modifier);
                                break;
                            case CommandLine:
                                handleSymbolWithRipple(symbol, modifier);
                                break;
                            case Mash:
                                // we shouldn't match any symbols in mash mode
                                throw new IllegalStateException();
                            default:
                                throw new IllegalStateException();
                        }
                    } else if (null != mode) {
                        sendInfoMessage();

                        logger.log(Level.INFO, "entered mode: " + mode);
                    } else {
                        logger.warning("transition without output symbol nor change of mode");
                    }
                }

                @Override
                public void handleLaserOn() {
                    sendLaserOnMessage();
                }

                @Override
                public void handleLaserOff() {
                    sendLaserOffMessage();
                }
            };

            keyer = new ChordedKeyer(handler);
        } catch (IOException e) {
            throw new DeviceInitializationException(e);
        }

        oscReceiver.register(EXO_TT_ERROR, new OscMessageHandler() {
            public void handle(OSCMessage message) {
                List<Object> args = message.getArguments();
                if (wrongArgs(EXO_TT_ERROR, 1, args.size())) {
                    return;
                }

                logger.log(Level.SEVERE, "error message from Typeatron: " + args.get(0));
            }
        });

        oscReceiver.register(EXO_TT_INFO, new OscMessageHandler() {
            public void handle(OSCMessage message) {
                List<Object> args = message.getArguments();
                if (wrongArgs(EXO_TT_INFO, 1, args.size())) {
                    return;
                }

                logger.log(Level.INFO, "info message from Typeatron: " + args.get(0));
            }
        });

        oscReceiver.register(EXO_TT_KEYS, new OscMessageHandler() {
            public void handle(final OSCMessage message) {
                List<Object> args = message.getArguments();
                if (wrongArgs(EXO_TT_KEYS, 1, args.size())) {
                    return;
                }

                try {
                    keyer.nextInputState(((String) args.get(0)).getBytes());
                } catch (Exception e) {
                    logger.log(Level.SEVERE, "failed to relay Typeatron input", e);
                }
            }
        });

        /*
        // user has "pointed with reference". This event occurs at the moment the laser turns on.
        oscReceiver.register(EXO_TT_LASER_EVENT, new OscMessageHandler() {
            public void handle(OSCMessage message) {
                System.out.println("got laser event");
                // TODO: use the recognition time parameter provided in the message
                long recognitionTime = System.currentTimeMillis();

                handlePointEvent(recognitionTime);
                logger.info("handled laser pointer event from typeatron");
            }
        });
        */

        oscReceiver.register(EXO_TT_PHOTO_DATA, new OscMessageHandler() {
            public void handle(OSCMessage message) {
                List<Object> args = message.getArguments();
                if (wrongArgs(EXO_TT_PHOTO_DATA, 7, args.size())) {
                    return;
                }

                // workaround for unavailable Xerces dependency:
                // make startTime and endTime into xsd:long instead of xsd:dateTime
                long startTime = ((Date) args.get(0)).getTime();
                long endTime = ((Date) args.get(1)).getTime();

                Integer numberOfMeasurements = (Integer) args.get(2);
                Float minValue = (Float) args.get(3);
                Float maxValue = (Float) args.get(4);
                Float mean = (Float) args.get(5);
                Float variance = (Float) args.get(6);

                ModelConnection mc = rippleSession.getModelConnection();
                try {
                    rippleSession.push(startTime,
                            endTime,
                            numberOfMeasurements,
                            minValue,
                            maxValue,
                            variance,
                            mean);
                } catch (RippleException e) {
                    logger.log(Level.SEVERE,
                            "Ripple error while pushing photoresistor observation: " + e.getMessage());
                    e.printStackTrace(System.err);
                }
            }
        });

        oscReceiver.register(EXO_TT_PING, new OscMessageHandler() {
            public void handle(OSCMessage message) {
                // note: currently, no argument is provided, or needed;
                // the ping is used by the Typeatron to notify the user of a connection
                logger.info("ping received from Typeatron. Replying.");
                sendPingReplyMessage();
            }
        });

        oscReceiver.register(EXO_TT_PING_REPLY, new OscMessageHandler() {
            public void handle(OSCMessage message) {
                // note: argument is ignored for now; in future, it could be used to synchronize clocks

                // we assume this reply is a response to the latest ping
                // TODO: we don't have to... why not send and receive latestPing in the message
                long delay = System.currentTimeMillis() - latestPing;

                logger.log(Level.INFO, "ping reply received from Typeatron in " + delay + "ms");
            }
        });

        // TODO: temporary... assume Emacs is available, even if we can't detect it...
        boolean forceEmacsAvailable = true;  // emacsAvailable

        try {
            brainModeWrapper = forceEmacsAvailable ? new BrainModeClientWrapper() : null;
        } catch (IOException e) {
            throw new DeviceInitializationException(e);
        }
    }

    public ExtendoAgent getAgent() {
        return agent;
    }

    public ChordedKeyer getKeyer() {
        return keyer;
    }

    public TypeatronMusicControl getMusic() {
        return music;
    }

    @Override
    protected void onConnect() {
        sendPingMessage();
    }

    private String symbolForBrainModeClient(final String symbol,
                                            final ChordedKeyer.Modifier modifier) {
        switch (modifier) {
            case Control:
                return symbol.length() == 1 ? "<C-" + symbol + ">" : "<" + symbol + ">";
            case None:
                return symbol.length() == 1 ? symbol : "<" + symbol + ">";
            default:
                throw new IllegalStateException();
        }
    }

    private void handleSymbolWithRipple(final String symbol,
                                        final ChordedKeyer.Modifier modifier) {
        try {
            rippleREPL.handle(symbol, modifier);
        } catch (RippleException e) {
            sendErrorMessage();
            logger.log(Level.WARNING, "Ripple error", e);
        }
    }

    private void handleSymbolWithEmacs(final String symbol,
                                       final ChordedKeyer.Modifier modifier) {
        String mapped = symbolForBrainModeClient(symbol, modifier);
        if (null != brainModeWrapper) {
            try {
                brainModeWrapper.write(mapped);
            } catch (IOException e) {
                sendErrorMessage();
                logger.log(Level.WARNING, "I/O error while writing to Brain-mode", e);
            }
        }
    }

    private class BrainModeClientWrapper {
        private boolean isAlive;
        private final PipedOutputStream source;

        public BrainModeClientWrapper() throws IOException {
            source = new PipedOutputStream();

            PipedInputStream sink = new PipedInputStream(source);
            BrainModeClient.ResultHandler resultHandler = new BrainModeClient.ResultHandler() {
                @Override
                public void handle(InputStream result) {
                    String s0;
                    try {
                        s0 = new String(IOUtil.readBytes(result)).trim();
                    } catch (IOException e) {
                        logger.log(Level.SEVERE, "error reading Brain-mode response", e);
                        sendErrorMessage();
                        return;
                    }
                    if (!s0.equals("nil")) {
                        // TODO: some future return values may need to be properly dequoted
                        String s1 = s0.substring(1, s0.length() - 1);
                        try {
                            rippleSession.push(s1);
                            sendOkMessage();
                        } catch (RippleException e) {
                            logger.log(Level.WARNING, "failed to push Brain-mode response", e);
                            sendWarningMessage();
                        }
                    }
                }
            };

            final BrainModeClient client = new BrainModeClient(sink, resultHandler);
            client.setExecutable(EMACSCLIENT_BIN);

            new Thread(new Runnable() {
                public void run() {
                    isAlive = true;

                    while (isAlive) {
                        try {
                            client.run();
                            isAlive = false;
                            sendWarningMessage();
                        } catch (BrainModeClient.ExecutionException e) {
                            logger.log(Level.WARNING,
                                    "Brain-mode client error: " + e.getMessage());
                            sendErrorMessage();
                        } catch (BrainModeClient.UnknownCommandException e) {
                            logger.log(Level.FINE,
                                    "unknown command: " + e.getMessage());
                            sendWarningMessage();
                        } catch (BrainModeClient.ParseError e) {
                            // attempt to recover from parse errors
                            sendErrorMessage();
                            logger.log(Level.SEVERE, "Brain-mode client parse error: ", e.getMessage());
                        } catch (Throwable t) {
                            isAlive = false;
                            logger.log(Level.SEVERE, "Brain-mode client thread died with error", t);
                            sendErrorMessage();
                        }
                    }
                }
            }).start();
        }

        public void write(final String symbol) throws IOException {
            source.write(symbol.getBytes());
        }
    }

    private long latestPing;

    public void sendErrorMessage() {
        OSCMessage m = new OSCMessage(EXO_TT_ERROR);
        send(m);
    }

    public void sendInfoMessage() {
        OSCMessage m = new OSCMessage(EXO_TT_INFO);
        send(m);
    }

    public void sendLaserFeedbackMessage() {
        System.out.println("laser feedback");
        OSCMessage m = new OSCMessage(EXO_TT_LASER_FEEDBACK);
        send(m);
    }

    public void sendLaserOffMessage() {
        System.out.println("laser off");
        thingPointedTo = null;
        OSCMessage m = new OSCMessage(EXO_TT_LASER_OFF);
        send(m);
    }

    public void sendLaserOnMessage() {
        System.out.println("laser on");

        // note: this moment slightly precedes the time at which the laser becomes visible,
        // assuming the message is received by the Typeatron
        long recognitionTime = System.currentTimeMillis();

        OSCMessage m = new OSCMessage(EXO_TT_LASER_ON);
        send(m);

        handlePointEvent(recognitionTime);
    }

    /*
    public void sendLaserTriggerMessage() {
        System.out.println("laser trigger");
        OSCMessage m = new OSCMessage(EXO_TT_LASER_TRIGGER);
        send(m);
    }*/

    public void sendMorseMessage(final String text) {
        OSCMessage m = new OSCMessage(EXO_TT_MORSE);
        m.addArgument(text);
        send(m);
    }

    public void sendOkMessage() {
        OSCMessage m = new OSCMessage(EXO_TT_OK);
        send(m);
    }

    public void sendPhotoGetMessage() {
        OSCMessage m = new OSCMessage(EXO_TT_PHOTO_GET);
        send(m);
    }

    public void sendPingMessage() {
        OSCMessage message = new OSCMessage(EXO_TT_PING);
        latestPing = System.currentTimeMillis();
        message.addArgument(latestPing);
        send(message);
    }

    public void sendPingReplyMessage() {
        OSCMessage message = new OSCMessage(EXO_TT_PING_REPLY);
        // note: currently, no argument is consumed by the Typeatron
        send(message);
    }

    public void sendReadyMessage() {
        OSCMessage m = new OSCMessage(EXO_TT_READY);
        send(m);
    }

    /**
     * @param time the duration of the signal in milliseconds (valid values range from 1 to 60000)
     */
    public void sendVibroMessage(final int time) {
        if (time < 0 || time > 60000) {
            throw new IllegalArgumentException("vibration interval too short or too long: " + time);
        }

        OSCMessage m = new OSCMessage(EXO_TT_VIBRO);
        m.addArgument(time);
        send(m);
    }

    public void sendWarningMessage() {
        OSCMessage m = new OSCMessage(EXO_TT_WARNING);
        send(m);
    }

    public void pointTo(final URI thingPointedTo) {
        // the next point event from the hardware will reference this thing
        this.thingPointedTo = thingPointedTo;

        //sendLaserTriggerMessage();
        keyer.setMode(ChordedKeyer.Mode.Laser);
    }

    private void handlePointEvent(final long recognitionTime) {
        timeOfLastEvent = recognitionTime;

        Date recognizedAt = new Date(recognitionTime);

        Dataset d = Activities.datasetForPointingGesture(recognizedAt.getTime(), agent.getAgentUri(), thingPointedTo);
        try {
            agent.sendDataset(d, Extendo.GESTURE_TTL);
        } catch (Exception e) {
            logger.log(Level.SEVERE, "failed to gestural dataset: " + e.getMessage());
            e.printStackTrace(System.err);
        }

        logger.log(Level.INFO, "pointed to " + thingPointedTo);
    }

    private boolean wrongArgs(final String address,
                              final int expected,
                              final int actual) {
        if (actual != expected) {
            logger.log(Level.SEVERE, "received " + actual + " arguments in " + address + " message, "
                    + "expected " + expected);
            return true;
        } else {
            return false;
        }
    }
}
