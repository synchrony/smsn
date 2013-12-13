package net.fortytwo.extendo.brainstem;

import android.util.Log;
import android.widget.EditText;
import com.illposed.osc.OSCMessage;
import net.fortytwo.extendo.brain.ExtendoBrain;
import net.fortytwo.rdfagents.model.Dataset;

import java.util.Date;

/**
 * @author Joshua Shinavier (http://fortytwo.net)
 */
public class ExtendoHandControl extends BluetoothDeviceControl {
    public ExtendoHandControl(final String address,
                              final OSCDispatcher oscDispatcher,
                              final ExtendoBrain brain,
                              final EventStackProxy proxy,
                              final BrainstemAgent agent,
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

                /*
                if (null != proxy) {
                    proxy.push(
                        brain.getEventStack().createGestureEvent(agent.getAgentUri().stringValue(), recognizedAt));
                }*/

                Dataset d = agent.datasetForGestureEvent(recognizedAt.getTime());
                try {
                    agent.getQueryEngine().addStatements(d.getStatements());
                    //agent.broadcastDataset(d);
                } catch (Exception e) {
                    Log.e(Brainstem.TAG, "failed to broadcast RDF dataset: " + e.getMessage());
                    e.printStackTrace(System.err);
                }
            }
        });
    }
}
