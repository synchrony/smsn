package net.fortytwo.extendo.monitron.listeners;

import com.illposed.osc.OSCMessage;
import net.fortytwo.extendo.monitron.Context;
import net.fortytwo.extendo.monitron.events.MonitronEvent;

/**
* @author Joshua Shinavier (http://fortytwo.net)
*/
public class SystemErrorListener extends MonitronListener {

    public SystemErrorListener(final Context context) {
        super(context);
    }

    protected MonitronEvent handleMessage(final OSCMessage m) throws MessageParseException {
        String message = arg(m, 0);
        System.err.println("monitron system error: " + message);

        // TODO
        return null;
    }
}
