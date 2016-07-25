package net.fortytwo.smsn.typeatron;

import com.illposed.osc.OSCMessage;
import info.aduna.io.IOUtil;
import net.fortytwo.rdfagents.model.Dataset;
import net.fortytwo.ripple.RippleException;
import net.fortytwo.smsn.SemanticSynchrony;
import net.fortytwo.smsn.SmSnDeviceControl;
import net.fortytwo.smsn.brain.BrainModeClient;
import net.fortytwo.smsn.p2p.SideEffects;
import net.fortytwo.smsn.p2p.SmSnAgent;
import net.fortytwo.smsn.p2p.osc.OscMessageHandler;
import net.fortytwo.smsn.p2p.osc.OscReceiver;
import net.fortytwo.smsn.rdf.Activities;
import net.fortytwo.smsn.typeatron.ripple.RippleSession;
import net.fortytwo.smsn.typeatron.ripple.SmSnRippleRepl;
import net.fortytwo.smsn.typeatron.ripple.lib.music.TypeatronMusicControl;
import net.fortytwo.stream.StreamProcessor;
import org.openrdf.model.IRI;
import org.openrdf.model.Value;
import org.openrdf.query.BindingSet;

import java.io.IOException;
import java.io.PipedInputStream;
import java.io.PipedOutputStream;
import java.util.Date;
import java.util.List;
import java.util.function.BiConsumer;
import java.util.logging.Level;
import java.util.logging.Logger;

/**
 * A controller for the Typeatron chorded keyer
 *
 * @author Joshua Shinavier (http://fortytwo.net)
 */
public class TypeatronControl extends SmSnDeviceControl {
    private static final Logger logger = Logger.getLogger(TypeatronControl.class.getName());
    
    // fully specified, since PATH may or may not include /usr/bin
    private static final String EMACSCLIENT_BIN = "/usr/bin/emacsclient";

    // outbound paths: for messages to the device
    private static final String
            OSC_LASER_FEEDBACK = "/laser/feedback",
            OSC_LASER_OFF = "/laser/off",
            OSC_LASER_ON = "/laser/on",
            OSC_PHOTO_GET = "/photo/get";

    // inbound paths: for messages from the device
    private static final String
            OSC_KEYS = "/keys",
            OSC_LASER_EVENT = "/laser/event",
            OSC_PHOTO_DATA = "/photo/data";

    public static final int
            VIBRATE_MANUAL_MS = 500;

    private final BrainModeClientWrapper brainModeWrapper;
    private final RippleSession rippleSession;
    private final SmSnRippleRepl rippleREPL;
    private final ChordedKeyer keyer;
    private final TypeatronMusicControl music;

    private IRI thingPointedTo;

    public TypeatronControl(final OscReceiver oscReceiver,
                            final SmSnAgent agent,
                            final SideEffects environment) throws DeviceInitializationException {
        super("/exo/tt", oscReceiver, agent);

        try {
            this.music = new TypeatronMusicControl();
        } catch (Exception e) {
            throw new DeviceInitializationException(e);
        }

        try {
            agent.getStreamProcessor().addQuery(QUERY_TTL, Activities.QUERY_FOR_ATTENTION,
                    new BiConsumer<BindingSet, Long>() {
                @Override
                public void accept(BindingSet b, Long expirationTime) {
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
        } catch (IOException | StreamProcessor.IncompatibleQueryException | StreamProcessor.InvalidQueryException e) {
            throw new DeviceInitializationException("failed to create Typeatron's query subscription", e);
        }

        try {
            rippleSession = new RippleSession();

            SmSnRippleRepl.REPLEventHandler eventHandler = new SmSnRippleRepl.REPLEventHandler() {
                @Override
                public void beginCommand() {
                    sendReadyMessage();
                }

                @Override
                public void finishCommand() {
                    sendOkMessage();
                }
            };
            rippleREPL = new SmSnRippleRepl(rippleSession, this, environment, eventHandler);
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
        
        oscReceiver.register(absoluteAddress(OSC_KEYS), new OscMessageHandler() {
            public void handle(final OSCMessage message) {
                List<Object> args = message.getArguments();
                if (wrongArgs(OSC_KEYS, 1, args.size())) {
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
        oscReceiver.register(absoluteAddress(OSC_LASER_EVENT), new OscMessageHandler() {
            public void handle(OSCMessage message) {
                System.out.println("got laser event");
                // TODO: use the recognition time parameter provided in the message
                long recognitionTime = System.currentTimeMillis();

                handlePointEvent(recognitionTime);
                logger.info("handled laser pointer event from typeatron");
            }
        });
        */

        oscReceiver.register(absoluteAddress(OSC_PHOTO_DATA), new OscMessageHandler() {
            public void handle(OSCMessage message) {
                List<Object> args = message.getArguments();
                if (wrongArgs(OSC_PHOTO_DATA, 7, args.size())) {
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

        // TODO: temporary... assume Emacs is available, even if we can't detect it...
        boolean forceEmacsAvailable = true;  // emacsAvailable

        try {
            brainModeWrapper = forceEmacsAvailable ? new BrainModeClientWrapper() : null;
        } catch (IOException e) {
            throw new DeviceInitializationException(e);
        }
    }

    public ChordedKeyer getKeyer() {
        return keyer;
    }

    public TypeatronMusicControl getMusic() {
        return music;
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
            BrainModeClient.ResultHandler resultHandler = result -> {
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
            };

            final BrainModeClient client = new BrainModeClient(sink, resultHandler);
            client.setExecutable(EMACSCLIENT_BIN);

            new Thread(() -> {
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
            }).start();
        }

        public void write(final String symbol) throws IOException {
            source.write(symbol.getBytes());
        }
    }

    public void sendLaserFeedbackMessage() {
        System.out.println("laser feedback");
        OSCMessage m = new OSCMessage(absoluteAddress(OSC_LASER_FEEDBACK));
        send(m);
    }

    public void sendLaserOffMessage() {
        System.out.println("laser off");
        thingPointedTo = null;
        OSCMessage m = new OSCMessage(absoluteAddress(OSC_LASER_OFF));
        send(m);
    }

    public void sendLaserOnMessage() {
        System.out.println("laser on");

        // note: this moment slightly precedes the time at which the laser becomes visible,
        // assuming the message is received by the Typeatron
        long recognitionTime = System.currentTimeMillis();

        OSCMessage m = new OSCMessage(absoluteAddress(OSC_LASER_ON));
        send(m);

        handlePointEvent(recognitionTime);
    }

    /*
    public void sendLaserTriggerMessage() {
        System.out.println("laser trigger");
        OSCMessage m = new OSCMessage(absoluteAddress(OSC_LASER_TRIGGER));
        send(m);
    }*/

    public void sendPhotoGetMessage() {
        OSCMessage m = new OSCMessage(absoluteAddress(OSC_PHOTO_GET));
        send(m);
    }

    public void pointTo(final IRI thingPointedTo) {
        // the next point event from the hardware will reference this thing
        this.thingPointedTo = thingPointedTo;

        //sendLaserTriggerMessage();
        keyer.setMode(ChordedKeyer.Mode.Laser);
    }

    private void handlePointEvent(final long recognitionTime) {
        timeOfLastEvent = recognitionTime;

        Date recognizedAt = new Date(recognitionTime);

        Dataset d = Activities.datasetForPointingGesture(recognizedAt.getTime(), agent.getAgentIri(), thingPointedTo);
        try {
            agent.sendDataset(d, SemanticSynchrony.GESTURE_TTL);
        } catch (Exception e) {
            logger.log(Level.SEVERE, "failed to send gestural dataset: " + e.getMessage());
            e.printStackTrace(System.err);
        }

        logger.log(Level.INFO, "pointed to " + thingPointedTo);
    }
}
