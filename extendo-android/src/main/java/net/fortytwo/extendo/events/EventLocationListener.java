package net.fortytwo.extendo.events;

import android.location.Location;
import android.location.LocationListener;
import android.os.Bundle;

/**
 * @author Joshua Shinavier (http://fortytwo.net)
 */
public class EventLocationListener implements LocationListener {
    private final EventNotifier notifier;

    public EventLocationListener() {
        this.notifier = EventNotifier.getInstance();
    }

    public void onLocationChanged(Location location) {
        notifier.addEvent("location changed: " + location);
    }

    public void onStatusChanged(String s, int i, Bundle bundle) {
        notifier.addEvent("status changed: " + s + ", " + i + ", " + bundle);
    }

    public void onProviderEnabled(String s) {
        notifier.addEvent("provider enabled: " + s);
    }

    public void onProviderDisabled(String s) {
        notifier.addEvent("provider disabled: " + s);
    }
}
