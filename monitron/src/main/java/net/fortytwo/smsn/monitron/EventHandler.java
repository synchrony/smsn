package net.fortytwo.smsn.monitron;

import net.fortytwo.smsn.monitron.events.MonitronEvent;

/**
 * @author Joshua Shinavier (http://fortytwo.net)
 */
public interface EventHandler {
    void handleEvent(final MonitronEvent e) throws EventHandler.EventHandlingException;

    static class EventHandlingException extends Exception {
        public EventHandlingException(final Throwable cause) {
            super(cause);
        }
    }
}
