package net.fortytwo.extendo;

import android.app.Activity;
import android.content.Context;
import android.content.Intent;
import android.location.LocationListener;
import android.location.LocationManager;
import android.os.Bundle;
import android.view.Menu;
import android.view.MenuInflater;
import android.view.MenuItem;
import android.view.View;
import android.view.View.OnClickListener;
import android.widget.EditText;
import net.fortytwo.extendo.events.EventLocationListener;
import net.fortytwo.extendo.events.EventsActivity;
import net.fortytwo.extendo.flashcards.android.Flashcards4Android;
import net.fortytwo.extendo.ping.BrainPingPopup;
import net.fortytwo.extendo.ping.BrainPingSettings;

public class Main extends Activity {
    private EditText editor;

    private final Activity thisActivity = this;

    public Main() {
    }

    /**
     * Called with the activity is first created.
     */
    @Override
    public void onCreate(final Bundle savedInstanceState) {
        super.onCreate(savedInstanceState);

        // Inflate our UI from its XML layout description.
        setContentView(R.layout.main_layout);

        // Find the text editor view inside the layout, because we
        // want to do various programmatic things with it.
        editor = (EditText) findViewById(R.id.editor);

        // Hook up button presses to the appropriate event handler.
        findViewById(R.id.back).setOnClickListener(backListener);
        findViewById(R.id.clear).setOnClickListener(clearListener);
        findViewById(R.id.flashcards).setOnClickListener(flashcardsListener);
        findViewById(R.id.events).setOnClickListener(eventsListener);

        editor.setText("testing");//getText(R.string.main_label));

        // Force the service to start.
        //     startService(new Intent(this, BrainPingService.class));

        LocationManager lm = (LocationManager) this.getSystemService(Context.LOCATION_SERVICE);
        LocationListener l = new EventLocationListener();
        lm.requestLocationUpdates(LocationManager.NETWORK_PROVIDER, 0, 0, l);
        lm.requestLocationUpdates(LocationManager.GPS_PROVIDER, 0, 0, l);
    }

    /**
     * Called when the activity is about to start interacting with the user.
     */
    @Override
    protected void onResume() {
        super.onResume();
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
     * A call-back for when the user presses the clear button.
     */
    private OnClickListener clearListener = new OnClickListener() {
        public void onClick(View v) {
            editor.setText("foo\nbar");

            startActivity(new Intent(thisActivity, BrainPingPopup.class));
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
}
