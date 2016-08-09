package net.fortytwo.smsn.monitron.listeners;

import com.illposed.osc.OSCMessage;
import net.fortytwo.smsn.monitron.Context;
import net.fortytwo.smsn.monitron.events.MonitronEvent;

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
