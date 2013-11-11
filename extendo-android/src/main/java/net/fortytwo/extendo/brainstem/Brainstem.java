package net.fortytwo.extendo.brainstem;

import android.content.BroadcastReceiver;
import android.content.Context;
import android.content.Intent;
import android.content.IntentFilter;
import android.media.AudioManager;
import android.media.ToneGenerator;
import android.util.Log;
import android.widget.EditText;
import at.abraxas.amarino.AmarinoIntent;
import com.illposed.osc.OSCMessage;
import com.illposed.osc.OSCPacket;
import com.illposed.osc.utility.OSCByteArrayToJavaConverter;

import java.io.File;
import java.io.FileInputStream;
import java.io.IOException;
import java.util.LinkedList;
import java.util.List;
import java.util.Properties;
import java.util.logging.Logger;

/**
 * @author Joshua Shinavier (http://fortytwo.net)
 */
public class Brainstem {
    public static final String TAG = "Brainstem";

    private static final Logger LOGGER = Logger.getLogger(Brainstem.class.getName());

    private static final String PROPS_PATH = "/sdcard/brainstem.props";

    private static final String
            PROP_EXTENDOHAND_ADDRESS = "net.fortytwo.extendo.hand.address";

    private final List<BluetoothDeviceControl> devices = new LinkedList<BluetoothDeviceControl>();

    private EditText textEditor;

    private final ArduinoReceiver arduinoReceiver = new ArduinoReceiver();

    public Brainstem() throws BrainstemException {
        loadConfiguration();
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

        // do never forget to unregister a registered receiver
        context.unregisterReceiver(arduinoReceiver);
    }

    // note: in Android, SharedPreferences are preferred to properties files.  This file specifically contains those
    // settings which change more frequently than the APK is loaded, such as network settings.
    // Ideally, this file will go away entirely once the Brainstem becomes reusable software rather than a
    // special-purpose component of a demo.
    private void loadConfiguration() throws BrainstemException {
        File conf = new File(PROPS_PATH);
        if (!conf.exists()) {
            throw new BrainstemException("configuration properties not found: " + PROPS_PATH);
        }

        Properties props = new Properties();
        try {
            props.load(new FileInputStream(conf));
        } catch (IOException e) {
            throw new BrainstemException(e);
        }

        String extendoHandAddress = props.getProperty(PROP_EXTENDOHAND_ADDRESS);
        if (null != extendoHandAddress) {
            BluetoothDeviceControl extendoHand = new ExtendoHandControl(extendoHandAddress);
            devices.add(extendoHand);
        }
    }

    public void addComplexEventHandler(final String query,
                                       final ComplexEventHandler handler) {
        // TODO
        // handle various continuous SPARQL results from facilitator

    }

    private void receiveOSCMessage(final String data,
                                   final OSCMessageHandler handler) {
        OSCByteArrayToJavaConverter c = new OSCByteArrayToJavaConverter();

        // TODO: is the array length all that is expected for the second argument?
        OSCPacket p = c.convert(data.getBytes(), data.getBytes().length);

        if (p instanceof OSCMessage) {
            handler.handle((OSCMessage) p);
        } else {
            LOGGER.warning("OSC packet is of non-message type " + p.getClass().getSimpleName() + ": " + p);
        }
    }

    public void setTextEditor(EditText textEditor) {
        this.textEditor = textEditor;
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

    private void playEventNotificationTone() {
        //startActivity(new Intent(thisActivity, PlaySound.class));

        tg.startTone(ToneGenerator.TONE_PROP_BEEP);
    }

    /**
     * ArduinoReceiver is responsible for catching broadcasted Amarino
     * events.
     * <p/>
     * It extracts data from the intent and updates the graph accordingly.
     */
    private class ArduinoReceiver extends BroadcastReceiver {

        @Override
        public void onReceive(Context context, Intent intent) {
            String data;

            // the device address from which the data was sent, we don't need it here but to demonstrate how you retrieve it
            final String address = intent.getStringExtra(AmarinoIntent.EXTRA_DEVICE_ADDRESS);

            //Log.i(TAG, "received from address: " + address);
            playEventNotificationTone();

            // the type of data which is added to the intent
            final int dataType = intent.getIntExtra(AmarinoIntent.EXTRA_DATA_TYPE, -1);

            // we only expect String data though, but it is better to check if really string was sent
            // later Amarino will support differnt data types, so far data comes always as string and
            // you have to parse the data to the type you have sent from Arduino, like it is shown below
            if (dataType == AmarinoIntent.STRING_EXTRA) {
                data = intent.getStringExtra(AmarinoIntent.EXTRA_DATA);

                if (data != null) {
                    Log.i(TAG, "received Extend-o-Hand data: " + data);
                    textEditor.setText("/exo/ " + data);
                }
            }


        }
    }
}
