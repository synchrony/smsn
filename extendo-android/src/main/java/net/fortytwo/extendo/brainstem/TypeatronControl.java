package net.fortytwo.extendo.brainstem;

import android.widget.EditText;
import com.illposed.osc.OSCMessage;

/**
 * @author Joshua Shinavier (http://fortytwo.net)
 */
public class TypeatronControl extends BluetoothDeviceControl {
    public TypeatronControl(final String address,
                            final OSCDispatcher oscDispatcher,
                            final EditText textEditor) {
        super(address);

        oscDispatcher.register("/exo/tt/keys", new OSCMessageHandler() {
            public void handle(final OSCMessage message) {
                Object[] args = message.getArguments();
                if (1 == args.length) {
                    textEditor.setText("Typeatron keys: " + args[0]);
                } else {
                    textEditor.setText("Typeatron error (wrong # of args)");
                }
            }
        });
    }
}
