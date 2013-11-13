package net.fortytwo.extendo.brainstem;

import android.content.Context;
import at.abraxas.amarino.Amarino;

/**
 * @author Joshua Shinavier (http://fortytwo.net)
 */
public abstract class BluetoothOSCDeviceControl {
    private final String address;
    private final String oscPrefix;

    public BluetoothOSCDeviceControl(final String address,
                                     final String oscPrefix) {
        this.address = address;
        this.oscPrefix = oscPrefix;
    }

    public void connect(final Context context) {
        // this is how you tell Amarino to connect to a specific BT device from within your own code
        Amarino.connect(context, address);
    }

    public void disconnect(final Context context) {
        // if you connect in onStart() you must not forget to disconnect when your app is closed
        Amarino.disconnect(context, address);
    }

    public String getOSCPrefix() {
        return oscPrefix;
    }

    public abstract void handleOSCStyleMessage(String message);
}
