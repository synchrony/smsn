package net.fortytwo.extendo.monitron;

import net.fortytwo.extendo.monitron.events.MonitronEvent;
import org.openrdf.model.ValueFactory;
import org.openrdf.model.impl.ValueFactoryImpl;

/**
 * @author Joshua Shinavier (http://fortytwo.net)
 */
public class Context {

    private final ValueFactory valueFactory = new ValueFactoryImpl();
    private final EventHandler eventHandler;

    private long timerStart;

    public Context(final EventHandler eventHandler) {
        this.eventHandler = eventHandler;
    }

    public ValueFactory getValueFactory() {
        return valueFactory;
    }

    public void handleEvent(final MonitronEvent e) throws EventHandler.EventHandlingException {
        eventHandler.handleEvent(e);
    }

    public void handleException(final Exception e) {
        System.err.println("exception thrown: " + e);
    }

    public void setTimerStart(final long time) {
        timerStart = time;
    }

    public long getTimerStart() {
        return timerStart;
    }

}
