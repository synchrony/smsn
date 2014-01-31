package net.fortytwo.extendo.brainstem;

import android.content.BroadcastReceiver;
import android.content.Context;
import android.content.Intent;
import android.content.IntentFilter;
import android.media.AudioFormat;
import android.media.AudioManager;
import android.media.AudioRecord;
import android.media.AudioTrack;
import android.util.Log;
import android.widget.EditText;
import at.abraxas.amarino.Amarino;
import at.abraxas.amarino.AmarinoIntent;
import com.illposed.osc.OSCMessage;
import com.illposed.osc.OSCPacket;
import com.illposed.osc.utility.OSCByteArrayToJavaConverter;
import com.tinkerpop.blueprints.KeyIndexableGraph;
import com.tinkerpop.blueprints.impls.tg.TinkerGraph;
import edu.rpi.twc.sesamestream.BindingSetHandler;
import edu.rpi.twc.sesamestream.QueryEngine;
import net.fortytwo.extendo.Extendo;
import net.fortytwo.extendo.Main;
import net.fortytwo.extendo.brain.BrainGraph;
import net.fortytwo.extendo.brain.ExtendoBrain;
import net.fortytwo.extendo.p2p.Pinger;
import net.fortytwo.extendo.util.properties.PropertyException;
import net.fortytwo.extendo.util.properties.TypedProperties;
import net.fortytwo.rdfagents.model.Dataset;
import org.openrdf.query.BindingSet;

import java.io.File;
import java.io.FileInputStream;
import java.io.IOException;
import java.util.Arrays;
import java.util.Date;
import java.util.LinkedList;
import java.util.List;
import java.util.Properties;

/**
 * @author Joshua Shinavier (http://fortytwo.net)
 */
public class Brainstem {
    public static final String TAG = "Brainstem";

    private static final String PROPS_PATH = "/sdcard/brainstem.props";

    public static final String
            PROP_AGENTURI = "net.fortytwo.extendo.agentUri",
            PROP_REXSTER_HOST = "net.fortytwo.extendo.rexster.host",
            PROP_REXSTER_PORT = "net.fortytwo.extendo.rexster.port",
            PROP_REXSTER_GRAPH = "net.fortytwo.extendo.rexster.graph",
            PROP_EXTENDOHAND_ADDRESS = "net.fortytwo.extendo.hand.address",
            PROP_TYPEATRON_ADDRESS = "net.fortytwo.extendo.typeatron.address";

    private final List<BluetoothDeviceControl> devices;

    private BluetoothDeviceControl extendoHand;
    private BluetoothDeviceControl typeatron;

    private final OSCDispatcher oscDispatcher;

    private EditText textEditor;

    private final ArduinoReceiver arduinoReceiver = new ArduinoReceiver();

    private final NotificationToneGenerator toneGenerator = new NotificationToneGenerator();

    private Properties configuration;

    private BrainstemAgent agent;
    private final ExtendoBrain brain;

    private Context context;
    private final Main.Toaster toaster;
    private boolean emacsAvailable;

    public Brainstem(final Main.Toaster toaster) throws ExtendoBrain.ExtendoBrainException {
        this.toaster = toaster;

        // TODO: this TinkerGraph is a temporary solution
        KeyIndexableGraph g = new TinkerGraph();
        BrainGraph bg = new BrainGraph(g);
        brain = new ExtendoBrain(bg);

        devices = new LinkedList<BluetoothDeviceControl>();
        oscDispatcher = new OSCDispatcher();
    }

    /**
     * Load and configure resources with dependencies which cannot be resolved at construction time,
     * such as (currently) the text editor
     */
    public void initialize(boolean emacsAvailable) throws BrainstemException {
        this.emacsAvailable = emacsAvailable;

        try {
            loadConfiguration();
        } catch (PropertyException e) {
            throw new BrainstemException(e);
        }
    }

    public void setTextEditor(final EditText textEditor) {
        this.textEditor = textEditor;
    }

    public void connect(final Context context) {
        this.context = context;

        // in order to receive broadcasted intents we need to register our receiver
        context.registerReceiver(arduinoReceiver, new IntentFilter(AmarinoIntent.ACTION_RECEIVED));

        for (BluetoothDeviceControl d : devices) {
            d.connect(context);
        }
    }

    public void disconnect(final Context context) {
        for (BluetoothDeviceControl d : devices) {
            d.disconnect(context);
        }

        // don't forget to unregister a registered receiver
        context.unregisterReceiver(arduinoReceiver);
    }

    // note: in Android, SharedPreferences are preferred to properties files.  This file specifically contains those
    // settings which change more frequently than the APK is loaded, such as network settings.
    // Ideally, this file will go away entirely once the Brainstem becomes reusable software rather than a
    // special-purpose component of a demo.
    private void loadConfiguration() throws BrainstemException, PropertyException {
        // this configuration is currently separate from the main Extendo configuration, which reads from
        // ./extendo.properties.  On Android, this path may be an inaccessible location (in the file system root)
        if (null == configuration) {
            File f = new File(PROPS_PATH);
            if (!f.exists()) {
                throw new BrainstemException("configuration properties not found: " + PROPS_PATH);
            }

            configuration = new TypedProperties();
            try {
                configuration.load(new FileInputStream(f));
            } catch (IOException e) {
                throw new BrainstemException(e);
            }
        }

        // temporary code to investigate merging brainstem.props with extendo.properties
        /*
        Log.i(TAG, "creating extendo.properties");
        try {
            File f = new File("extendo.properties");
            Log.i(TAG, "f.getAbsolutePath(): " + f.getAbsolutePath());
            Log.i(TAG, "f.exists(): " + f.exists());
            if (!f.exists()) {
                f.createNewFile();
                OutputStream fout = new FileOutputStream(f);
                fout.write("foo".getBytes());
                fout.close();
            }
        } catch (IOException e) {
            throw new BrainstemException(e);
        }
        */

        String rexsterHost = configuration.getProperty(PROP_REXSTER_HOST);
        String rexsterPort = configuration.getProperty(PROP_REXSTER_PORT);
        String rexsterGraph = configuration.getProperty(PROP_REXSTER_GRAPH);
        if (null == rexsterHost || null == rexsterPort || null == rexsterGraph) {
            throw new BrainstemException("Rexster endpoint info is missing from configuration: use "
                    + PROP_REXSTER_HOST + ", " + PROP_REXSTER_PORT + ", and " + PROP_REXSTER_GRAPH);
        }

        String endpoint = "http://" + rexsterHost + ":" + rexsterPort + "/graphs/" + rexsterGraph + "/extendo/";

        EventStackProxy proxy = new EventStackProxy(endpoint + "push-event");

        // note: currently, setTextEditor() must be called before passing textEditor to the device controls

        String u = configuration.getProperty(PROP_AGENTURI);
        if (null == u) {
            throw new BrainstemException("who are you? Missing value for " + PROP_AGENTURI);
        } else {
            try {
                agent = new BrainstemAgent(u);
                Log.i(TAG, "just created BrainstemAgent with URI " + u);

                final BindingSetHandler queryAnswerHandler = new BindingSetHandler() {
                    public void handle(final BindingSet bindings) {
                        long delay = System.currentTimeMillis() - agent.timeOfLastEvent;

                        toneGenerator.play();

                        toaster.makeText("latency (before tone) = " + delay + "ms");

                        Log.i(Brainstem.TAG, "received SPARQL query result: " + bindings);
                    }
                };

                agent.getQueryEngine().addQuery(BrainstemAgent.QUERY_FOR_ALL_GB_GESTURES, queryAnswerHandler);
            } catch (QueryEngine.InvalidQueryException e) {
                throw new BrainstemException(e);
            } catch (IOException e) {
                throw new BrainstemException(e);
            } catch (QueryEngine.IncompatibleQueryException e) {
                throw new BrainstemException(e);
            }
        }

        String extendoHandAddress = configuration.getProperty(PROP_EXTENDOHAND_ADDRESS);
        if (null != extendoHandAddress) {
            Log.i(TAG, "loading Extend-o-Hand device at address " + extendoHandAddress);
            extendoHand
                    = new ExtendoHandControl(extendoHandAddress, oscDispatcher, brain, proxy, agent, textEditor, toaster);
            addBluetoothDevice(extendoHand);
        }

        String typeatronAddress = configuration.getProperty(PROP_TYPEATRON_ADDRESS);
        if (null != typeatronAddress) {
            Log.i(TAG, "loading Typeatron device at address " + typeatronAddress);
            try {
                typeatron
                        = new TypeatronControl(typeatronAddress, oscDispatcher, textEditor, toaster, emacsAvailable);
            } catch (BluetoothDeviceControl.DeviceInitializationException e) {
                throw new BrainstemException(e);
            }
            addBluetoothDevice(typeatron);
        }
    }

    private void addBluetoothDevice(final BluetoothDeviceControl dc) {
        devices.add(dc);
    }

    private void handleOSCData(final String data) {
        OSCByteArrayToJavaConverter c = new OSCByteArrayToJavaConverter();

        // TODO: is the array length all that is expected for the second argument?
        OSCPacket p = c.convert(data.getBytes(), data.getBytes().length);

        if (p instanceof OSCMessage) {
            if (!oscDispatcher.dispatch((OSCMessage) p)) {
                Log.w(TAG, "no OSC handler at address " + ((OSCMessage) p).getAddress());

                // TODO: temporary debugging code
                String address = ((OSCMessage) p).getAddress();
                StringBuilder sb = new StringBuilder("address bytes:");
                for (byte b : address.getBytes()) {
                    sb.append(" ").append((int) b);
                }
                Log.w(TAG, sb.toString());
            }
        } else {
            Log.w(TAG, "OSC packet is of non-message type " + p.getClass().getSimpleName() + ": " + p);
        }

        /*
        int i = message.indexOf(" ");
        if (i > 0) {
            String prefix = message.substring(i);

            BluetoothOSCDeviceControl dc = deviceByOSCPrefix.get(prefix);
            if (null == dc) {
                Log.w(TAG, "no control matching OSC address " + prefix);
            } else {
                dc.handleOSCStyleMessage(message);
            }
        }
        */
    }

    public class BrainstemException extends Exception {
        public BrainstemException(final String message) {
            super(message);
        }

        public BrainstemException(final Throwable cause) {
            super(cause);
        }
    }

    public class NotificationToneGenerator {
        // Note: is it possible to generate a tone with lower latency than this default generator's?
        //private final ToneGenerator tg = new ToneGenerator(AudioManager.STREAM_NOTIFICATION, 100);

        private final AudioTrack audioTrack;
        private final int minSize;
        private final float synth_frequency = 880;
        private final int sampleRate;

        public NotificationToneGenerator() {
            sampleRate = getValidSampleRate();

            minSize = AudioTrack.getMinBufferSize(sampleRate,
                    AudioFormat.CHANNEL_OUT_MONO,
                    AudioFormat.ENCODING_PCM_16BIT);
            audioTrack = new AudioTrack(AudioManager.STREAM_MUSIC,
                    sampleRate,
                    AudioFormat.CHANNEL_OUT_MONO,
                    AudioFormat.ENCODING_PCM_16BIT,
                    minSize,
                    AudioTrack.MODE_STREAM);
        }

        public void play() {
            //startActivity(new Intent(thisActivity, PlaySound.class));

            //tg.startTone(ToneGenerator.TONE_PROP_BEEP);

            audioTrack.play();
            short[] buffer = new short[minSize];
            float angle = 0;
            //while (true) {
            //    if (play) {
            for (int i = 0; i < buffer.length; i++) {
                float angular_frequency =
                        (float) (2 * Math.PI) * synth_frequency / sampleRate;
                buffer[i] = (short) (Short.MAX_VALUE * ((float) Math.sin(angle)));
                angle += angular_frequency;
            }
            audioTrack.write(buffer, 0, buffer.length);
            //    }
            //}
        }

        private int getValidSampleRate() {
            for (int rate : new int[]{8000, 11025, 16000, 22050, 44100}) {  // add the rates you wish to check against
                int bufferSize = AudioRecord.getMinBufferSize(rate, AudioFormat.CHANNEL_CONFIGURATION_DEFAULT, AudioFormat.ENCODING_PCM_16BIT);
                if (bufferSize > 0) {
                    return rate;
                }
            }

            throw new IllegalStateException("could not find a valid sample rate for audio output");
        }
    }

    // TODO: temporary
    public void sendTestMessageToTypeatron(final Context context) {
        OSCMessage m = new OSCMessage();
        m.setAddress("/exo/tt/rgb");
        m.addArgument(0xff0000);

        typeatron.sendOSCMessage(m);
    }

    public void simulateGestureEvent() {
        agent.timeOfLastEvent = System.currentTimeMillis();

        Date recognizedAt = new Date();

        Dataset d = agent.datasetForGestureEvent(recognizedAt.getTime());
        try {
            agent.getQueryEngine().addStatements(d.getStatements());
            //agent.broadcastDataset(d);
        } catch (Exception e) {
            Log.e(Brainstem.TAG, "failed to broadcast RDF dataset: " + e.getMessage());
            e.printStackTrace(System.err);
        }
    }

    public void pingFacilitatorConnection() {
        try {
            agent.getPinger().ping(new Pinger.PingResultHandler() {
                public void handleResult(long delay) {
                    toaster.makeText("ping delay: " + delay + "ms");
                }
            });
        } catch (Throwable t) {
            Log.e(TAG, "error pinging connection: " + t.getMessage());
            t.printStackTrace(System.err);
        }
    }

    public void pingExtendoHand() {
        agent.timeOfLastEvent = System.currentTimeMillis();
        // note: the timestamp argument is not yet used
        Amarino.sendDataToArduino(context, extendoHand.getAddress(), 'p', agent.timeOfLastEvent);
    }

    /**
     * ArduinoReceiver is responsible for catching broadcasted Amarino
     * events.
     * <p/>
     * It extracts data from the intent and updates the graph accordingly.
     */
    private class ArduinoReceiver extends BroadcastReceiver {

        @Override
        public void onReceive(final Context context,
                              final Intent intent) {

            // the device address from which the data was sent, we don't need it here but to demonstrate how you retrieve it
            String address = intent.getStringExtra(AmarinoIntent.EXTRA_DEVICE_ADDRESS);

            //Log.i(TAG, "received from address: " + address);

            //toneGenerator.play();

            // the type of data which is added to the intent
            int dataType = intent.getIntExtra(AmarinoIntent.EXTRA_DATA_TYPE, -1);

            // we only expect String data though, but it is better to check if really string was sent
            // later Amarino will support differnt data types, so far data comes always as string and
            // you have to parse the data to the type you have sent from Arduino, like it is shown below
            if (dataType == AmarinoIntent.STRING_EXTRA) {
                String data = intent.getStringExtra(AmarinoIntent.EXTRA_DATA);

                if (data != null) {
                    if (0 == data.length()) {
                        Log.w(TAG, "received zero-length message from Arduino");
                    } else {
                        if (Extendo.VERBOSE) {
                            Log.i(TAG, "data from Arduino: " + data);
                        }
                        //textEditor.setText("OSC: " + data);

                        /*
                        // TODO: temporary debugging code
                        StringBuilder sb = new StringBuilder("data:");
                        for (byte b : data.getBytes()) {
                            sb.append(" ").append((int) b);
                        }
                        Log.i(TAG, sb.toString());
                        //*/

                        byte[] bytes = data.getBytes();

                        // strip off the odd 0xEF 0xBF 0xBD three-byte sequence which sometimes encloses the message
                        // I haven't quite grokked it.  It's like a UTF-8 byte order mark, but not quite, and it appears
                        // both at the end and the beginning of the message.
                        // It appears only when Amarino *and* OSCuino are used to send the OSC data over Bluetooth
                        if (bytes.length >= 6) {
                            //Log.i(TAG, "bytes[0] = " + (int) bytes[0] + ", " + "bytes[bytes.length - 3] = " + bytes[bytes.length - 3]);

                            if (-17 == bytes[0] && -17 == bytes[bytes.length - 3]) {
                                data = new String(Arrays.copyOfRange(bytes, 3, bytes.length - 3));
                            }

                            if (0 == data.length()) {
                                Log.w(TAG, "received empty message from Arduino");
                                return;
                            }
                        }

                        // TODO: catching ArrayIndexOutOfBoundsException is temporary; fix the problem in the Arduino
                        try {
                            handleOSCData(data);
                        } catch (ArrayIndexOutOfBoundsException e) {
                            Log.e(Brainstem.TAG, "array index out of bounds when reading from Arduino");
                        }
                    }
                }
            }
        }
    }
}
