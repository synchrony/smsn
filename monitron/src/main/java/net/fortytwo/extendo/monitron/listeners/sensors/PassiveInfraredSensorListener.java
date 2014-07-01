package net.fortytwo.extendo.monitron.listeners.sensors;

import com.illposed.osc.OSCMessage;
import net.fortytwo.extendo.monitron.Context;
import net.fortytwo.extendo.monitron.data.BooleanData;
import net.fortytwo.extendo.monitron.events.MonitronEvent;
import net.fortytwo.extendo.monitron.events.MotionObservation;
import org.openrdf.model.URI;

/**
 * @author Joshua Shinavier (http://fortytwo.net)
 */
public class PassiveInfraredSensorListener extends SensorListener {

    public PassiveInfraredSensorListener(final Context context,
                                            final URI sensor) {
        super(context, sensor);
    }

    protected MonitronEvent handleMessage(final OSCMessage m) throws MessageParseException {
        BooleanData s = new BooleanData();

        int i = 0;

        s.setSampleIntervalBeginning(timeArg(m, i++));
        s.setSampleIntervalEnd(timeArg(m, i++));
        s.setTotalMeasurements(longArg(m, i++));
        s.setResult(booleanArg(m, i));

        return handleSample(s);
    }

    protected MonitronEvent handleSample(final BooleanData data) {
        return new MotionObservation(context, sensor, data);
    }
}
