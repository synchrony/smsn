package net.fortytwo.extendo.brainstem;

/**
 * @author Joshua Shinavier (http://fortytwo.net)
 */
public class ExtendoHandControl extends BluetoothOSCDeviceControl {
    public ExtendoHandControl(final String address) {
        super(address, "/exo/hand");
    }

    public void handleOSCStyleMessage(final String message) {
        // TODO
    }
}
