package net.fortytwo.extendo.brainstem;

/**
 * @author Joshua Shinavier (http://fortytwo.net)
 */
public class TypeatronControl extends BluetoothOSCDeviceControl {
    public TypeatronControl(final String address) {
        super(address, "/exo/tt");
    }

    public void handleOSCStyleMessage(final String message) {
        // TODO
    }
}
