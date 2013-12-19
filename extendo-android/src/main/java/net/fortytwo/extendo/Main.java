package net.fortytwo.extendo;

import android.app.Activity;
import android.app.ActivityManager;
import android.content.Context;
import android.content.Intent;
import android.location.LocationListener;
import android.location.LocationManager;
import android.os.Bundle;
import android.util.Log;
import android.view.Menu;
import android.view.MenuInflater;
import android.view.MenuItem;
import android.view.View;
import android.view.View.OnClickListener;
import android.widget.EditText;
import android.widget.Toast;
import net.fortytwo.extendo.brain.ExtendoBrain;
import net.fortytwo.extendo.brainstem.Brainstem;
import net.fortytwo.extendo.events.EventLocationListener;
import net.fortytwo.extendo.events.EventsActivity;
import net.fortytwo.extendo.flashcards.android.Flashcards4Android;
import net.fortytwo.extendo.ping.BrainPingSettings;

/**
 * @author Joshua Shinavier (http://fortytwo.net)
 */
public class Main extends Activity {
    private EditText editor;

    private final Activity thisActivity = this;

    private final Brainstem brainstem;

    private final Toaster toaster = new Toaster();

    public Main() throws ExtendoBrain.ExtendoBrainException {
        brainstem = new Brainstem(toaster);
    }

    /**
     * Called with the activity is first created.
     */
    @Override
    public void onCreate(final Bundle savedInstanceState) {
        super.onCreate(savedInstanceState);

        Log.i(Brainstem.TAG, "Brainstem create()");

        // Inflate our UI from its XML layout description.
        setContentView(R.layout.main_layout);

        // Find the text editor view inside the layout, because we
        // want to do various programmatic things with it.
        editor = (EditText) findViewById(R.id.editor);
        brainstem.setTextEditor(editor);

        // Hook up button presses to the appropriate event handler.
        findViewById(R.id.back).setOnClickListener(backListener);
        findViewById(R.id.tryme).setOnClickListener(trymeListener);
        findViewById(R.id.ping).setOnClickListener(pingListener);
        findViewById(R.id.btPing).setOnClickListener(bluetoothPingListener);
        findViewById(R.id.flashcards).setOnClickListener(flashcardsListener);
        findViewById(R.id.events).setOnClickListener(eventsListener);

        editor.setText("testing");
        checkForEmacs();

        // Force the service to start.
        //     startService(new Intent(this, BrainPingService.class));

        LocationManager lm = (LocationManager) this.getSystemService(Context.LOCATION_SERVICE);
        LocationListener l = new EventLocationListener();
        lm.requestLocationUpdates(LocationManager.NETWORK_PROVIDER, 0, 0, l);
        lm.requestLocationUpdates(LocationManager.GPS_PROVIDER, 0, 0, l);

        try {
            brainstem.initialize();
        } catch (Brainstem.BrainstemException e) {
            Log.e(Brainstem.TAG, "exception in Brainstem initialization: " + e.getMessage());
        }
    }

    @Override
    protected void onStart() {
        super.onStart();

        Log.i(Brainstem.TAG, "Brainstem start()");

        brainstem.connect(this);
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

        Log.i(Brainstem.TAG, "Brainstem stop()");

        brainstem.disconnect(this);
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

    private final Context context = this;

    private OnClickListener trymeListener = new OnClickListener() {

        public void onClick(View v) {

            editor.setText("simulating gesture event");
            brainstem.simulateGestureEvent();

            /*
            editor.setText("");
            editor.setText("sending a message\nto the Typeatron");
            //brainstem.playEventNotificationTone();
            brainstem.sendTestMessageToTypeatron(context);
            //*/

            //startActivity(new Intent(thisActivity, BrainPingPopup.class));
        }
    };

    private OnClickListener pingListener = new OnClickListener() {

        public void onClick(View v) {
            editor.setText("pinging facilitator connection");
            brainstem.pingFacilitatorConnection();
        }
    };

    private OnClickListener bluetoothPingListener = new OnClickListener() {

        public void onClick(View v) {
            editor.setText("pinging bluetooth device");
            brainstem.pingBluetooth();
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

        /*
        List< ActivityManager.RunningTaskInfo > taskInfo = am.getRunningTasks(1);

        Log.d("topActivity", "CURRENT Activity ::"
                + taskInfo.get(0).topActivity.getClassName());

        ComponentName ci = taskInfo.get(0).topActivity;
        ci.getPackageName();
        */
    }

    // a helper object which allows Toasts to be displayed from non-UI threads
    public class Toaster {
        public void makeText(final String message) {
            Main.this.runOnUiThread(new Runnable() {
                public void run() {
                    Toast.makeText(Main.this, message, Toast.LENGTH_SHORT).show();
                }
            });
        }
    }
}
