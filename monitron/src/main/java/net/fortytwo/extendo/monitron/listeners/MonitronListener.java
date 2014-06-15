package net.fortytwo.extendo.monitron.listeners;

import com.illposed.osc.OSCListener;
import com.illposed.osc.OSCMessage;
import net.fortytwo.extendo.monitron.Context;
import net.fortytwo.extendo.monitron.EventHandler;
import net.fortytwo.extendo.monitron.events.Event;

import java.util.Date;
import java.util.List;

/**
 * @author Joshua Shinavier (http://fortytwo.net)
 */
public abstract class MonitronListener implements OSCListener {
    protected final Context context;

    protected MonitronListener(Context context) {
        this.context = context;
    }

    protected abstract Event handleMessage(OSCMessage m) throws MessageParseException;

    public void acceptMessage(final Date date,
                              final OSCMessage m) {
        try {
            Event event = handleMessage(m);
            if (null != event) {
                context.handleEvent(event);
            }
        } catch (MessageParseException e) {
            handleParseError(e);
        } catch (EventHandler.EventHandlingException e) {
            handleEventHandlingError(e);
        }
    }

    protected String arg(final OSCMessage m,
                         final int index) throws MessageParseException {
        if (index < 0) {
            throw new IllegalArgumentException();
        }

        List args = m.getArguments();

        if (index >= args.size()) {
            throw new MessageParseException("not enough arguments");
        }

        Object a = args.get(index);

        if (!(a instanceof String)) {
            throw new MessageParseException("string-typed arguments expected");
        }

        return (String) a;
    }

    protected double doubleArg(final OSCMessage m,
                               final int index) throws MessageParseException {
        String a = arg(m, index);

        try {
            return Double.valueOf(a);
        } catch (NumberFormatException e) {
            throw new MessageParseException("invalid floating-point number: " + a);
        }
    }

    protected long longArg(final OSCMessage m,
                           final int index) throws MessageParseException {
        String a = arg(m, index);

        try {
            return Long.valueOf(a);
        } catch (NumberFormatException e) {
            throw new MessageParseException("invalid long integer: " + a);
        }
    }

    protected long hexLongArg(final OSCMessage m,
                              final int index) throws MessageParseException {
        String a = arg(m, index);

        try {
            return Long.parseLong(a.toLowerCase(), 16);
        } catch (NumberFormatException e) {
            throw new MessageParseException("invalid hexadecimal long integer: " + a);
        }
    }

    protected long timeArg(final OSCMessage m,
                           final int index) throws MessageParseException {
        long l = hexLongArg(m, index);

        return context.getTimerStart() + l;
    }

    protected boolean booleanArg(final OSCMessage m,
                                 final int index) throws MessageParseException {
        long b = longArg(m, index);

        if (1 == b) {
            return true;
        } else if (0 == b) {
            return false;
        } else {
            throw new MessageParseException("boolean value should be equal to 0 or 1: " + b);
        }
    }

    protected void handleParseError(final MessageParseException e) {
        context.handleException(e);
    }

    protected void handleEventHandlingError(final EventHandler.EventHandlingException e) {
        context.handleException(e);
    }

    public static class MessageParseException extends Exception {
        public MessageParseException(final String message) {
            super(message);
        }
    }
}
