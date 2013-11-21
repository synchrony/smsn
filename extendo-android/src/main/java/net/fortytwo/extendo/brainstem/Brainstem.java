package net.fortytwo.extendo.brainstem;

import android.content.BroadcastReceiver;
import android.content.Context;
import android.content.Intent;
import android.content.IntentFilter;
import android.media.AudioManager;
import android.media.ToneGenerator;
import android.util.Log;
import android.widget.EditText;
import at.abraxas.amarino.Amarino;
import at.abraxas.amarino.AmarinoIntent;
import com.illposed.osc.OSCMessage;
import com.illposed.osc.OSCPacket;
import com.illposed.osc.utility.OSCByteArrayToJavaConverter;
import com.tinkerpop.blueprints.KeyIndexableGraph;
import com.tinkerpop.blueprints.impls.tg.TinkerGraph;
import net.fortytwo.extendo.brain.BrainGraph;
import net.fortytwo.extendo.brain.ExtendoBrain;

import java.io.File;
import java.io.FileInputStream;
import java.io.IOException;
import java.util.Arrays;
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
            PROP_EXTENDOHAND_ADDRESS = "net.fortytwo.extendo.hand.address",
            PROP_TYPEATRON_ADDRESS = "net.fortytwo.extendo.typeatron.address";

    private final List<BluetoothDeviceControl> devices;

    private BluetoothDeviceControl typeatron;

    private final OSCDispatcher oscDispatcher;

    private EditText textEditor;

    private final ArduinoReceiver arduinoReceiver = new ArduinoReceiver();

    private Properties configuration;

    private BrainstemAgent agent;
    private final ExtendoBrain brain;

    public Brainstem() throws ExtendoBrain.ExtendoBrainException {
        // TODO: this TinkerGraph is a temporary solution
        KeyIndexableGraph g = new TinkerGraph();
        BrainGraph bg = new BrainGraph(g);
        brain = new ExtendoBrain(bg);

        devices = new LinkedList<BluetoothDeviceControl>();
        oscDispatcher = new OSCDispatcher();
    }

    /**
     * Supplies a Brainstem configuration.
     * Otherwise, the configuration will be loaded from the default location on disk: /sdcard/brainstem.props
     * @param conf the configuration properties
     */
    public void setConfiguration(final Properties conf) {
        configuration = conf;
    }

    /**
     * Load and configure resources with dependencies which cannot be resolved at construction time,
     * such as (currently) the text editor
     */
    public void initialize() throws BrainstemException {
        loadConfiguration();
    }

    public BrainstemAgent getAgent() {
        return agent;
    }

    public void setTextEditor(EditText textEditor) {
        this.textEditor = textEditor;
    }

    public void connect(final Context context) {
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
    private void loadConfiguration() throws BrainstemException {
        if (null == configuration) {
            File f = new File(PROPS_PATH);
            if (!f.exists()) {
                throw new BrainstemException("configuration properties not found: " + PROPS_PATH);
            }

            configuration = new Properties();
            try {
                configuration.load(new FileInputStream(f));
            } catch (IOException e) {
                throw new BrainstemException(e);
            }
        }

        // note: currently, setTextEditor() must be called before passing textEditor to the device controls

        String u = configuration.getProperty(PROP_AGENTURI);
        if (null == u) {
            throw new BrainstemException("who are you? Missing value for " + PROP_AGENTURI);
        } else {
            agent = new BrainstemAgent(u);
        }

        String extendoHandAddress = configuration.getProperty(PROP_EXTENDOHAND_ADDRESS);
        if (null != extendoHandAddress) {
            Log.i(TAG, "loading Extend-o-Hand device at address " + extendoHandAddress);
            BluetoothDeviceControl extendoHand
                    = new ExtendoHandControl(extendoHandAddress, oscDispatcher, brain, agent.getAgentUri().stringValue(), textEditor);
            addBluetoothDevice(extendoHand);
        }

        String typeatronAddress = configuration.getProperty(PROP_TYPEATRON_ADDRESS);
        if (null != typeatronAddress) {
            Log.i(TAG, "loading Typeatron device at address " + typeatronAddress);
            typeatron
                    = new TypeatronControl(typeatronAddress, oscDispatcher, textEditor);
            addBluetoothDevice(typeatron);
        }
    }

    public void addComplexEventHandler(final String query,
                                       final ComplexEventHandler handler) {
        // TODO
        // handle various continuous SPARQL results from facilitator

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

    // Note: is it possible to generate a tone with lower latency than this default generator's?
    final ToneGenerator tg = new ToneGenerator(AudioManager.STREAM_NOTIFICATION, 100);

    public void playEventNotificationTone() {
        //startActivity(new Intent(thisActivity, PlaySound.class));

        tg.startTone(ToneGenerator.TONE_PROP_BEEP);


        //Instrumentation m_Instrumentation = new Instrumentation();
        //m_Instrumentation.sendKeyDownUpSync(KeyEvent.KEYCODE_B);
    }

    // TODO: temporary
    public void sendTestMessageToTypeatron(final Context context) {
        OSCMessage m = new OSCMessage();
        m.setAddress("/exo/tt/rgb");
        m.addArgument(0xff0000);

        Amarino.sendDataToArduino(context, typeatron.getAddress(), 'e', m.getByteArray());
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
            playEventNotificationTone();

            // the type of data which is added to the intent
            int dataType = intent.getIntExtra(AmarinoIntent.EXTRA_DATA_TYPE, -1);

            // we only expect String data though, but it is better to check if really string was sent
            // later Amarino will support differnt data types, so far data comes always as string and
            // you have to parse the data to the type you have sent from Arduino, like it is shown below
            if (dataType == AmarinoIntent.STRING_EXTRA) {
                String data = intent.getStringExtra(AmarinoIntent.EXTRA_DATA);

                if (data != null) {
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
                    }

                    Log.i(TAG, "data from Arduino: " + data);
                    //textEditor.setText("OSC: " + data);

                    /*
                    // TODO: temporary debugging code
                    StringBuilder sb = new StringBuilder("data:");
                    for (byte b : data.getBytes()) {
                        sb.append(" ").append((int) b);
                    }
                    Log.i(TAG, sb.toString());
                    */

                    handleOSCData(data);
                }
            }
        }
    }
}
