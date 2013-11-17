package net.fortytwo.extendo.brainstem;

import android.content.Context;
import at.abraxas.amarino.Amarino;

/**
 * @author Joshua Shinavier (http://fortytwo.net)
 */
public abstract class BluetoothDeviceControl {
    private final String address;

    public BluetoothDeviceControl(final String address) {
        this.address = address;
    }

    public String getAddress() {
        return address;
    }

    public void connect(final Context context) {
        // this is how you tell Amarino to connect to a specific BT device from within your own code
        Amarino.connect(context, address);
    }

    public void disconnect(final Context context) {
        // if you connect in onStart() you must not forget to disconnect when your app is closed
        Amarino.disconnect(context, address);
    }
}
