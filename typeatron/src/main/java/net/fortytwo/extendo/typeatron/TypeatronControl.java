package net.fortytwo.extendo.typeatron;

import com.illposed.osc.OSCMessage;
import net.fortytwo.extendo.brain.BrainModeClient;
import net.fortytwo.extendo.p2p.ExtendoAgent;
import net.fortytwo.extendo.p2p.osc.OSCDispatcher;
import net.fortytwo.extendo.p2p.osc.OSCMessageHandler;
import net.fortytwo.extendo.p2p.osc.SlipOscControl;
import net.fortytwo.extendo.rdf.Gesture;
import net.fortytwo.extendo.typeatron.ripple.Environment;
import net.fortytwo.extendo.typeatron.ripple.ExtendoRippleREPL;
import net.fortytwo.extendo.typeatron.ripple.RippleSession;
import net.fortytwo.rdfagents.model.Dataset;
import net.fortytwo.ripple.RippleException;
import net.fortytwo.ripple.model.ModelConnection;
import org.openrdf.model.URI;

import java.io.IOException;
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
public class TypeatronControl extends SlipOscControl {

    protected static final Logger logger = Logger.getLogger(TypeatronControl.class.getName());

    // outbound addresses
    private static final String
            EXO_TT_LASER_TRIGGER = "/exo/tt/laser/trigger",
            EXO_TT_MODE = "/exo/tt/mode",
            EXO_TT_MORSE = "/exo/tt/morse",
            EXO_TT_PHOTO_GET = "/exo/tt/photo/get",
            EXO_TT_PING = "/exo/tt/ping",
            EXO_TT_VIBR = "/exo/tt/vibr";

    // inbound addresses
    private static final String
            EXO_TT_ERROR = "/exo/tt/error",
            EXO_TT_INFO = "/exo/tt/info",
            EXO_TT_KEYS = "/exo/tt/keys",
            EXO_TT_LASER_EVENT = "/exo/tt/laser/event",
            EXO_TT_PHOTO_DATA = "/exo/tt/photo/data",
            EXO_TT_PING_REPLY = "/exo/tt/ping/reply";

    public static final int
            VIBRATE_ALERT_MS = 250,
            VIBRATE_MANUAL_MS = 500;

    private final ExtendoAgent agent;
    private final Environment environment;

    private final BrainModeClientWrapper brainModeWrapper;
    private final RippleSession rippleSession;
    private final ExtendoRippleREPL rippleREPL;
    private final ChordedKeyer keyer;

    private URI thingPointedTo;

    public TypeatronControl(final OSCDispatcher oscDispatcher,
                            final ExtendoAgent agent,
                            final Environment environment) throws DeviceInitializationException {
        super(oscDispatcher);

        this.agent = agent;
        this.environment = environment;

        try {
            rippleSession = new RippleSession(agent, environment);
            rippleREPL = new ExtendoRippleREPL(rippleSession, this, agent, environment);
        } catch (RippleException e) {
            throw new DeviceInitializationException(e);
        }

        try {
            keyer = new ChordedKeyer(new ChordedKeyer.EventHandler() {
                public void handle(ChordedKeyer.Mode mode, String symbol, ChordedKeyer.Modifier modifier) {
                    if (null != symbol) {
                        try {
                            rippleREPL.handle(symbol, modifier, mode);
                        } catch (RippleException e) {
                            logger.log(Level.WARNING, "Ripple error: " + e.getMessage());
                            e.printStackTrace(System.err);
                        }

                        String mod = modifySymbol(symbol, modifier);
                        //toaster.makeText("typed: " + mod);
                        if (null != brainModeWrapper) {
                            try {
                                brainModeWrapper.write(mod);
                            } catch (IOException e) {
                                logger.log(Level.WARNING, "I/O error while writing to Brain-mode: " + e.getMessage());
                                e.printStackTrace(System.err);
                            }
                        }
                    } else {
                        sendModeInfo(mode);

                        logger.log(Level.INFO, "entered mode: " + mode);
                    }
                }
            });
        } catch (IOException e) {
            throw new DeviceInitializationException(e);
        }

        oscDispatcher.register(EXO_TT_ERROR, new OSCMessageHandler() {
            public void handle(OSCMessage message) {
                List<Object> args = message.getArguments();
                if (1 == args.size()) {
                    logger.log(Level.SEVERE, "error message from Typeatron: " + args.get(0));
                } else {
                    logger.log(Level.SEVERE, "wrong number of arguments in Typeatron error message");
                }
            }
        });

        oscDispatcher.register(EXO_TT_INFO, new OSCMessageHandler() {
            public void handle(OSCMessage message) {
                List<Object> args = message.getArguments();
                if (1 == args.size()) {
                    logger.log(Level.INFO, "info message from Typeatron: " + args.get(0));
                } else {
                    logger.log(Level.SEVERE, "wrong number of arguments in Typeatron info message");
                }
            }
        });

        oscDispatcher.register(EXO_TT_KEYS, new OSCMessageHandler() {
            public void handle(final OSCMessage message) {
                List<Object> args = message.getArguments();
                if (1 == args.size()) {
                    try {
                        keyer.nextInputState(((String) args.get(0)).getBytes());
                    } catch (Exception e) {
                        logger.log(Level.SEVERE, "failed to relay Typeatron input");
                        e.printStackTrace(System.err);
                    }
                } else {
                    logger.log(Level.SEVERE, "Typeatron control error (wrong # of args)");
                }
            }
        });

        oscDispatcher.register(EXO_TT_PHOTO_DATA, new OSCMessageHandler() {
            public void handle(OSCMessage message) {
                List<Object> args = message.getArguments();
                if (7 != args.size()) {
                    throw new IllegalStateException("photoresistor observation has unexpected number of arguments ("
                            + args.size() + "): " + message);
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
                    rippleSession.push(mc.valueOf(startTime),
                            mc.valueOf(endTime),
                            mc.valueOf(numberOfMeasurements),
                            mc.valueOf(minValue),
                            mc.valueOf(maxValue),
                            mc.valueOf(variance),
                            mc.valueOf(mean));
                } catch (RippleException e) {
                    logger.log(Level.SEVERE,
                            "Ripple error while pushing photoresistor observation: " + e.getMessage());
                    e.printStackTrace(System.err);
                }
            }
        });

        oscDispatcher.register(EXO_TT_PING_REPLY, new OSCMessageHandler() {
            public void handle(OSCMessage message) {
                // note: argument is ignored for now; in future, it could be used to synchronize clocks

                // we assume this reply is a response to the latest ping
                // TODO: we don't have to... why not send and receive latestPing in the message
                long delay = System.currentTimeMillis() - latestPing;

                logger.log(Level.INFO, "ping reply received from Typeatron in " + delay + "ms");
            }
        });

        oscDispatcher.register(EXO_TT_LASER_EVENT, new OSCMessageHandler() {
            public void handle(OSCMessage message) {
                // TODO: use the recognition time parameter provided in the message
                long recognitionTime = System.currentTimeMillis();

                handlePointEvent(recognitionTime);
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

    @Override
    protected void onConnect() {
        sendPing();
    }

    private String modifySymbol(final String symbol,
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

    private class BrainModeClientWrapper {
        private boolean isAlive;
        private final PipedOutputStream source;

        public BrainModeClientWrapper() throws IOException {
            source = new PipedOutputStream();

            PipedInputStream sink = new PipedInputStream(source);

            final BrainModeClient client = new BrainModeClient(sink);
            // since /usr/bin may not be in PATH
            client.setExecutable("/usr/bin/emacsclient");

            new Thread(new Runnable() {
                public void run() {
                    isAlive = true;

                    try {
                        client.run();
                    } catch (BrainModeClient.ExecutionException e) {
                        logger.log(Level.SEVERE,
                                "Brain-mode client giving up due to execution exception: " + e.getMessage());
                    } catch (Throwable t) {
                        logger.log(Level.SEVERE, "Brain-mode client thread died with error: " + t.getMessage());
                        t.printStackTrace(System.err);
                    } finally {
                        isAlive = false;
                    }
                }
            }).start();
        }

        public void write(final String symbol) throws IOException {
            logger.log(Level.INFO, (isAlive ? "" : "NOT ") + "writing '" + symbol + "' to Emacs...");
            source.write(symbol.getBytes());
        }
    }

    private long latestPing;

    public void sendPing() {
        OSCMessage message = new OSCMessage(EXO_TT_PING);
        latestPing = System.currentTimeMillis();
        message.addArgument(latestPing);
        send(message);
    }

    // feedback to the Typeatron whenever mode changes
    public void sendModeInfo(final ChordedKeyer.Mode mode) {
        OSCMessage m = new OSCMessage(EXO_TT_MODE);
        m.addArgument(mode.name());
        send(m);
    }

    public void sendLaserTriggerCommand() {
        OSCMessage m = new OSCMessage(EXO_TT_LASER_TRIGGER);
        send(m);
    }

    public void sendMorse(final String text) {
        OSCMessage m = new OSCMessage(EXO_TT_MORSE);
        m.addArgument(text);
        send(m);
    }

    public void sendPhotoresistorGetCommand() {
        OSCMessage m = new OSCMessage(EXO_TT_PHOTO_GET);
        send(m);
    }

    public void pointTo(final URI thingPointedTo) {
        // the next point event from the hardware will reference this thing
        this.thingPointedTo = thingPointedTo;

        sendLaserTriggerCommand();
    }

    private void handlePointEvent(final long recognitionTime) {
        timeOfLastEvent = recognitionTime;

        Date recognizedAt = new Date(recognitionTime);

        Dataset d = Gesture.datasetForPointingGesture(recognizedAt.getTime(), agent.getAgentUri(), thingPointedTo);
        try {
            agent.sendDataset(d, environment.verbose());
        } catch (Exception e) {
            logger.log(Level.SEVERE, "failed to gestural dataset: " + e.getMessage());
            e.printStackTrace(System.err);
        }

        logger.log(Level.INFO, "pointed to " + thingPointedTo);
    }

    /**
     * @param time the duration of the signal in milliseconds (valid values range from 1 to 60000)
     */
    public void sendVibrateCommand(final int time) {
        if (time < 0 || time > 60000) {
            throw new IllegalArgumentException("vibration interval too short or too long: " + time);
        }

        OSCMessage m = new OSCMessage(EXO_TT_VIBR);
        m.addArgument(time);
        send(m);
    }
}
