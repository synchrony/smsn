package net.fortytwo.extendo.monitron;

import net.fortytwo.extendo.monitron.events.Event;
import org.openrdf.model.ValueFactory;
import org.openrdf.model.impl.ValueFactoryImpl;

/**
 * @author Joshua Shinavier (http://fortytwo.net)
 */
public class EventHandler {

    private final ValueFactory valueFactory = new ValueFactoryImpl();

    public ValueFactory getValueFactory() {
        return valueFactory;
    }

    public void handleEvent(final Event e) throws EventHandlingException {
        ...
    }

    public void handleException(final Exception e) {
        ...
    }

    // secondary events ////////////////

    public void noiseEvent() {

    }

    public void newMotionEvent() {

    }

    public void highDustOrSmokeEvent() {

    }

    public void lightsSwitchedOffEvent() {

    }

    public void lightsSwitchedOnEvent() {

    }

    // TODO: distinguish between different kinds of change
    //       drop and stay down
    //       spike and stay high
    //       drop and return
    //       spike and return
    public void suddenPressureChangeEvent() {

    }

    // tertiary events /////////////////

    public void personPresentEvent() {

    }

    public void fireEvent() {

    }

    public void windowOrDoorOpenedEvent() {

    }

    public void windowOrDoorClosedEvent() {

    }

    ////////////////////////////////////

    public static class EventHandlingException extends Exception {

    }
}
