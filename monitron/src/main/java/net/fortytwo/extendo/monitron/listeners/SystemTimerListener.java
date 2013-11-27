package net.fortytwo.extendo.monitron.listeners;

import com.illposed.osc.OSCMessage;
import net.fortytwo.extendo.monitron.Context;
import net.fortytwo.extendo.monitron.events.Event;

/**
 * @author Joshua Shinavier (http://fortytwo.net)
 */
public class SystemTimerListener extends MonitronListener {

    public SystemTimerListener(final Context context) {
        super(context);
    }

    protected Event handleMessage(final OSCMessage m) throws MessageParseException {
        long time = hexLongArg(m, 0);

        long now = System.currentTimeMillis();

        context.setTimerStart(now - time);

        //System.out.println("time: " + new Date(time) + ", set to " + new Date(now - time));

        return null;  // TODO
    }
}
