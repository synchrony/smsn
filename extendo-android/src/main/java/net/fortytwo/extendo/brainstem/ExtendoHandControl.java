package net.fortytwo.extendo.brainstem;

import android.widget.EditText;
import com.illposed.osc.OSCMessage;
import net.fortytwo.extendo.brain.ExtendoBrain;

import java.util.Date;

/**
 * @author Joshua Shinavier (http://fortytwo.net)
 */
public class ExtendoHandControl extends BluetoothDeviceControl {
    public ExtendoHandControl(final String address,
                              final OSCDispatcher oscDispatcher,
                              final ExtendoBrain brain,
                              final EventStackProxy proxy,
                              final String agentUri,
                              final EditText textEditor) {
        super(address);

        oscDispatcher.register("/exo/hand/raw", new OSCMessageHandler() {
            public void handle(final OSCMessage message) {

                // TODO: the recognition instant should be inferred from the timestamp supplied by the device
                Date recognizedAt = new Date();

                Object[] args = message.getArguments();
                if (5 == args.length) {
                    textEditor.setText("Extend-o-Hand raw gesture: " + args[0] + " " + args[1] + " " + args[2] + " " + args[3] + " " + args[4]);
                } else {
                    textEditor.setText("Extend-o-Hand error (wrong # of args)");
                }

                if (null != proxy) {
                    proxy.push(
                        brain.getEventStack().createGestureEvent(agentUri, recognizedAt));
                }
            }
        });
    }
}
