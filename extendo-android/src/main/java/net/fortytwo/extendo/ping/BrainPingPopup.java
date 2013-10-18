package net.fortytwo.extendo.ping;

import android.app.Activity;
import android.app.AlertDialog;
import android.app.Dialog;
import android.app.NotificationManager;
import android.content.Context;
import android.content.DialogInterface;
import android.os.Bundle;
import android.util.Log;
import net.fortytwo.extendo.R;

/**
 * User: josh
 * Date: 1/12/11
 * Time: 5:05 PM
 */
public class BrainPingPopup extends Activity {
    private static final String LOG_TAG = BrainPingPopup.class.getName();

    @Override
    public void onCreate(Bundle savedInstanceState) {
        super.onCreate(savedInstanceState);
        setContentView(R.layout.info_layout);

        // TODO: is there a better place to discard the notification?
        String ns = Context.NOTIFICATION_SERVICE;
        NotificationManager manager = (NotificationManager) getSystemService(ns);
        manager.cancel(BrainPingService.BRAINPING_ID);

        showDialog(0);
    }

    @Override
    protected Dialog onCreateDialog(int id) {
        AlertDialog.Builder builder = new AlertDialog.Builder(this);
        builder.setMessage("Brain ping!")
                .setCancelable(false)
                .setPositiveButton("Acknowledge", new DialogInterface.OnClickListener() {
                    public void onClick(DialogInterface dialog, int id) {
                        acknowledgeBrainPing();
                    }
                }).setNegativeButton("Postpone", new DialogInterface.OnClickListener() {
            public void onClick(DialogInterface dialog, int id) {
                postponeBrainPing();
            }
        });
        return builder.create();
    }

    private void acknowledgeBrainPing() {
        Log.i(LOG_TAG, "brain ping acknowledged");
        // Do nothing.
        finish();
    }

    private void postponeBrainPing() {
        Log.i(LOG_TAG, "brain ping postponed");
        // Do nothing.
        finish();
    }
}
