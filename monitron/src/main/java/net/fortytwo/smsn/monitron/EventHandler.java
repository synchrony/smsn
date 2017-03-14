package net.fortytwo.smsn.monitron;

import net.fortytwo.smsn.monitron.events.MonitronEvent;

public interface EventHandler {
    void handleEvent(final MonitronEvent e) throws EventHandlingException;

    class EventHandlingException extends Exception {
        public EventHandlingException(final Throwable cause) {
            super(cause);
        }
    }
}
