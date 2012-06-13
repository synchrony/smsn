package net.fortytwo.extendo.hand;

import com.illposed.osc.OSCListener;
import com.illposed.osc.OSCMessage;
import com.illposed.osc.OSCPortIn;

/**
 * @author Joshua Shinavier (http://fortytwo.net)
 */
public class OscPlay {
    public static void main(final String[] args) throws Exception {
//        OSCPortIn receiver = new OSCPortIn(OSCPort.defaultSCOSCPort());
        int port = 1331;
        System.out.println("listening on port " + port);
        OSCPortIn receiver = new OSCPortIn(port);
        OSCListener listener = new OSCListener() {
            public void acceptMessage(java.util.Date time, OSCMessage message) {
                //System.out.println("Message received: " + message);
                StringBuilder sb = new StringBuilder();
                for (Object a : message.getArguments()) {
                    sb.append("\t").append(a);
                }
                System.out.println(sb);
            }
        };
//        receiver.addListener("/p5glove_data", listener);
        receiver.addListener("/p5-out", listener);
        receiver.startListening();
    }
}
