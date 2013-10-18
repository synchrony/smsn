package net.fortytwo.extendo.ping;

import android.app.IntentService;
import android.app.Notification;
import android.app.NotificationManager;
import android.app.PendingIntent;
import android.app.Service;
import android.content.Context;
import android.content.Intent;
import android.content.SharedPreferences;
import android.os.IBinder;
import android.preference.PreferenceManager;
import android.widget.Toast;
import net.fortytwo.extendo.R;

import java.util.Collection;
import java.util.LinkedList;


/**
 * User: josh
 * Date: 1/12/11
 * Time: 5:40 PM
 */
public class BrainPingService extends IntentService {
    public static final int BRAINPING_ID = 1;

    private static final Service INSTANCE;

    static {
        INSTANCE = new BrainPingService();
    }

    private final Collection<Pinger> pingers;

    public BrainPingService() {
        super("brain ping service");
        pingers = new LinkedList<Pinger>();
        this.add(new DeepThoughtsPinger(getApplicationContext()));

        Toast.makeText(this, "BrainPingService constructor", Toast.LENGTH_LONG).show();

        System.out.println("BrainPingService has been instantiated");
    }

    public void add(final Pinger p) {
        pingers.add(p);
    }

    public static Service getInstance() {
        return INSTANCE;
    }

    private void doNotification() {
        String ns = Context.NOTIFICATION_SERVICE;
        NotificationManager manager = (NotificationManager) getSystemService(ns);

        // TODO: change the icon
        int icon = R.drawable.ic_menu_info_details;

        CharSequence tickerText = "ping!";
        long when = System.currentTimeMillis();

        Notification note = new Notification(icon, tickerText, when);

        Context context = getApplicationContext();
        CharSequence contentTitle = "Brain ping";
        CharSequence contentText = "Time to think deep thoughts";
        Intent notificationIntent = new Intent(this, BrainPingPopup.class);
        PendingIntent contentIntent = PendingIntent.getActivity(this, 0, notificationIntent, 0);

        note.setLatestEventInfo(context, contentTitle, contentText, contentIntent);
        manager.notify(BRAINPING_ID, note);
    }

    @Override
    public IBinder onBind(final Intent intent) {
        // Clients cannot bind to this service.
        return null;
    }

    @Override
    protected void onHandleIntent(Intent intent) {
        //To change body of implemented methods use File | Settings | File Templates.
    }

    @Override
    public void onCreate() {
        Toast.makeText(this, "Service Created", Toast.LENGTH_LONG).show();

        // FIXME
        Collection<Pinger> pingers = null;

        SharedPreferences prefs = PreferenceManager.getDefaultSharedPreferences(getApplicationContext());
        new BrainPingScheduler(prefs, new Runnable() {
            public void run() {
                doNotification();
            }
        }, pingers);
    }

    @Override
    public void onDestroy() {
    }

    @Override
    public void onStart(Intent intent, int startid) {
        System.out.println("BrainPingService has been started");
        //doNotification();
    }
}
