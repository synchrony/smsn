package net.fortytwo.extendo.monitron.listeners.sensors;

import com.illposed.osc.OSCMessage;
import net.fortytwo.extendo.monitron.MonitronEventHandler;
import net.fortytwo.extendo.monitron.data.BooleanData;
import net.fortytwo.extendo.monitron.events.Event;
import net.fortytwo.extendo.monitron.events.MotionObservation;
import org.openrdf.model.URI;

/**
 * @author Joshua Shinavier (http://fortytwo.net)
 */
public class PassiveInfraredSensorListener extends SensorListener {

    public PassiveInfraredSensorListener(final MonitronEventHandler context,
                                            final URI sensor) {
        super(context, sensor);
    }

    protected Event handleMessage(final OSCMessage m) throws MessageParseException {
        BooleanData s = new BooleanData();

        int i = 0;

        s.setSampleIntervalBeginning(timeArg(m, i++));
        s.setSampleIntervalEnd(timeArg(m, i++));
        s.setTotalMeasurements(longArg(m, i++));
        s.setResult(booleanArg(m, i));

        return handleSample(s);
    }

    protected Event handleSample(final BooleanData data) {
        return new MotionObservation(context, sensor, data);
    }
}
