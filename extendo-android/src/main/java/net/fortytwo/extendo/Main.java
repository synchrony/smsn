package net.fortytwo.extendo;

import android.app.Activity;
import android.app.ActivityManager;
import android.content.BroadcastReceiver;
import android.content.Context;
import android.content.Intent;
import android.content.IntentFilter;
import android.location.LocationListener;
import android.location.LocationManager;
import android.media.AudioManager;
import android.media.ToneGenerator;
import android.os.Bundle;
import android.util.Log;
import android.view.Menu;
import android.view.MenuInflater;
import android.view.MenuItem;
import android.view.View;
import android.view.View.OnClickListener;
import android.widget.EditText;
import at.abraxas.amarino.Amarino;
import at.abraxas.amarino.AmarinoIntent;
import net.fortytwo.extendo.events.EventLocationListener;
import net.fortytwo.extendo.events.EventsActivity;
import net.fortytwo.extendo.flashcards.android.Flashcards4Android;
import net.fortytwo.extendo.ping.BrainPingSettings;

public class Main extends Activity {
    private EditText editor;

    private static final String TAG = "Brainstem";

    // change this to your Bluetooth device address
    private static final String DEVICE_ADDRESS = "00:06:66:46:C3:42"; // BlueSMIRF Silver #1

    private ArduinoReceiver arduinoReceiver = new ArduinoReceiver();

    private final Activity thisActivity = this;

    public Main() {
    }

    /**
     * Called with the activity is first created.
     */
    @Override
    public void onCreate(final Bundle savedInstanceState) {
        super.onCreate(savedInstanceState);

        Log.i(TAG, "Brainstem create()");

        // Inflate our UI from its XML layout description.
        setContentView(R.layout.main_layout);

        // Find the text editor view inside the layout, because we
        // want to do various programmatic things with it.
        editor = (EditText) findViewById(R.id.editor);

        // Hook up button presses to the appropriate event handler.
        findViewById(R.id.back).setOnClickListener(backListener);
        findViewById(R.id.tryme).setOnClickListener(trymeListener);
        findViewById(R.id.flashcards).setOnClickListener(flashcardsListener);
        findViewById(R.id.events).setOnClickListener(eventsListener);

        editor.setText("testing");//getText(R.string.main_label));
        checkForEmacs();

        // Force the service to start.
        //     startService(new Intent(this, BrainPingService.class));

        LocationManager lm = (LocationManager) this.getSystemService(Context.LOCATION_SERVICE);
        LocationListener l = new EventLocationListener();
        lm.requestLocationUpdates(LocationManager.NETWORK_PROVIDER, 0, 0, l);
        lm.requestLocationUpdates(LocationManager.GPS_PROVIDER, 0, 0, l);
    }

    @Override
    protected void onStart() {
        super.onStart();

        Log.i(TAG, "Brainstem start()");

        // in order to receive broadcasted intents we need to register our receiver
        registerReceiver(arduinoReceiver, new IntentFilter(AmarinoIntent.ACTION_RECEIVED));

        // this is how you tell Amarino to connect to a specific BT device from within your own code
        Amarino.connect(this, DEVICE_ADDRESS);
    }

    /**
     * Called when the activity is about to start interacting with the user.
     */
    @Override
    protected void onResume() {
        super.onResume();
    }

    @Override
    protected void onStop() {
        super.onStop();

        Log.i(TAG, "Brainstem stop()");

        // if you connect in onStart() you must not forget to disconnect when your app is closed
        Amarino.disconnect(this, DEVICE_ADDRESS);

        // do never forget to unregister a registered receiver
        unregisterReceiver(arduinoReceiver);
    }

    /**
     * Called when your activity's options menu needs to be created.
     */
    @Override
    public boolean onCreateOptionsMenu(Menu menu) {
        super.onCreateOptionsMenu(menu);

        MenuInflater inflater = getMenuInflater();
        inflater.inflate(R.menu.main_menu, menu);
        return true;
    }

    /**
     * Called right before your activity's option menu is displayed.
     */
    @Override
    public boolean onPrepareOptionsMenu(Menu menu) {
        super.onPrepareOptionsMenu(menu);

        return true;
    }

    /**
     * Called when a menu item is selected.
     */
    @Override
    public boolean onOptionsItemSelected(MenuItem item) {
        switch (item.getItemId()) {
            case R.id.info:
                startActivity(new Intent(this, Info.class));
                System.out.println("info_layout!");
                return true;
            case R.id.settings:
                startActivity(new Intent(this, BrainPingSettings.class));
                System.out.println("settings!");
                return true;
            default:
                return super.onOptionsItemSelected(item);
        }
    }

    /**
     * A call-back for when the user presses the back button.
     */
    private OnClickListener backListener = new OnClickListener() {
        public void onClick(View v) {
            finish();
        }
    };

    /**
     * A call-back for when the user presses the "try me" button.
     */
    private OnClickListener trymeListener = new OnClickListener() {
        public void onClick(View v) {
            editor.setText("you pressed\na button");

            playEventNotificationTone();

            //startActivity(new Intent(thisActivity, BrainPingPopup.class));
        }
    };

    private OnClickListener flashcardsListener = new OnClickListener() {
        public void onClick(View v) {
            startActivity(new Intent(thisActivity, Flashcards4Android.class));
        }
    };

    private OnClickListener eventsListener = new OnClickListener() {
        public void onClick(View v) {
            startActivity(new Intent(thisActivity, EventsActivity.class));
        }
    };

    // Note: is it possible to generate a tone with lower latency than this default generator's?
    final ToneGenerator tg = new ToneGenerator(AudioManager.STREAM_NOTIFICATION, 100);

    private void playEventNotificationTone() {
        //startActivity(new Intent(thisActivity, PlaySound.class));

        tg.startTone(ToneGenerator.TONE_PROP_BEEP);
    }

    // experimental method
    /*
    private void simulateKeypress() {
        editor.getCurrentInputConnection().sendKeyEvent(

                new KeyEvent(KeyEvent.ACTION_DOWN, a));
    }*/

    private void checkForEmacs() {
        ActivityManager.RunningAppProcessInfo emacs = null;
        ActivityManager am = (ActivityManager) this.getSystemService(ACTIVITY_SERVICE);
        for (ActivityManager.RunningAppProcessInfo p : am.getRunningAppProcesses()) {
            if (p.processName.equals("com.zielm.emacs")) {
                emacs = p;
                break;
            }
        }

        if (null != emacs) {
            editor.setText("Emacs is running");
        } else {
            editor.setText("Emacs is not running");
        }
    }

    /**
     * ArduinoReceiver is responsible for catching broadcasted Amarino
     * events.
     * <p/>
     * It extracts data from the intent and updates the graph accordingly.
     */
    public class ArduinoReceiver extends BroadcastReceiver {

        @Override
        public void onReceive(Context context, Intent intent) {
            String data = null;

            // the device address from which the data was sent, we don't need it here but to demonstrate how you retrieve it
            final String address = intent.getStringExtra(AmarinoIntent.EXTRA_DEVICE_ADDRESS);

            Log.i(TAG, "received from address: " + address);
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
                    editor.setText(data);
                    try {
                        // since we know that our string value is an int number we can parse it to an integer
                        //final int sensorReading = Integer.parseInt(data);
                        //mGraph.addDataPoint(sensorReading);
                    } catch (NumberFormatException e) { /* oh data was not an integer */ }
                }
            }


        }
    }
}
