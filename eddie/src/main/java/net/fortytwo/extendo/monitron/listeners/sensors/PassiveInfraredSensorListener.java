package net.fortytwo.extendo.monitron.listeners.sensors;

import com.illposed.osc.OSCMessage;
import net.fortytwo.extendo.monitron.EventHandler;
import net.fortytwo.extendo.monitron.events.Event;
import net.fortytwo.extendo.monitron.events.MotionObservation;
import net.fortytwo.extendo.monitron.data.BooleanData;

/**
 * @author Joshua Shinavier (http://fortytwo.net)
 */
public class PassiveInfraredSensorListener extends SensorListener {

    protected PassiveInfraredSensorListener(EventHandler context) {
        super(context, sensor);
    }

    protected Event handleMessage(final OSCMessage m) throws MessageParseException {
        BooleanData s = new BooleanData();

        int i = 0;

        s.setStartTime(longArg(m, i++));
        s.setEndTime(longArg(m, i++));
        s.setTotalMeasurements(longArg(m, i++));
        s.setResult(booleanArg(m, i));

        return handleSample(s);
    }

    protected Event handleSample(BooleanData s) {
        return new MotionObservation(context, s);
    }
}
