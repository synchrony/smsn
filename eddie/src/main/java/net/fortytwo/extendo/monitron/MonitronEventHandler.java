package net.fortytwo.extendo.monitron;

import net.fortytwo.extendo.monitron.events.Event;
import net.fortytwo.sesametools.nquads.NQuadsWriter;
import org.openrdf.model.Statement;
import org.openrdf.model.ValueFactory;
import org.openrdf.model.impl.ValueFactoryImpl;
import org.openrdf.rio.RDFHandlerException;
import org.openrdf.rio.RDFWriter;

/**
 * @author Joshua Shinavier (http://fortytwo.net)
 */
public class MonitronEventHandler {

    private final ValueFactory valueFactory = new ValueFactoryImpl();
    private long timerStart;

    public ValueFactory getValueFactory() {
        return valueFactory;
    }

    public void handleEvent(final Event e) throws EventHandlingException {
        System.out.println("\nreceived dataset:\t\n\n");
        RDFWriter w = new NQuadsWriter(System.out);
        try {
            w.startRDF();
            for (Statement s : e.getDataset().getStatements()) {
                w.handleStatement(s);
            }
            w.endRDF();
        } catch (RDFHandlerException e1) {
            throw new EventHandlingException(e1);
        }
    }

    public void handleException(final Exception e) {
        System.err.println("exception thrown: " + e);
    }

    public void setTimerStart(final long offset) {
        timerStart = offset;
    }

    public long getTimerStart() {
        return timerStart;
    }

    public static class EventHandlingException extends Exception {
        public EventHandlingException(final Throwable cause) {
            super(cause);
        }
    }
}
