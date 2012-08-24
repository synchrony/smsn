package net.fortytwo.extendo.monitron;

import net.fortytwo.extendo.monitron.events.Event;

/**
 * @author Joshua Shinavier (http://fortytwo.net)
 */
public interface EventHandler {
    void handleEvent(final Event e) throws EventHandler.EventHandlingException;

    static class EventHandlingException extends Exception {
        public EventHandlingException(final Throwable cause) {
            super(cause);
        }
    }
}
