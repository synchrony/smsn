package net.fortytwo.extendo.monitron;

import net.fortytwo.extendo.monitron.events.MonitronEvent;

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
