package net.fortytwo.extendo.monitron.listeners.sensors;

import com.illposed.osc.OSCMessage;
import net.fortytwo.extendo.monitron.EventHandler;
import net.fortytwo.extendo.monitron.events.Event;
import net.fortytwo.extendo.monitron.data.AnalogData;
import org.openrdf.model.URI;

/**
 * @author Joshua Shinavier (http://fortytwo.net)
 */
public abstract class AnalogSensorListener extends SensorListener {

    protected AnalogSensorListener(EventHandler context,
                                   final URI sensor) {
        super(context, sensor);
    }

    protected abstract Event handleSample(AnalogData s);

    protected Event handleMessage(OSCMessage m) throws MessageParseException {

        AnalogData s = new AnalogData();

        int i = 0;

        s.setStartTime(longArg(m, i++));
        s.setEndTime(longArg(m, i++));
        s.setTotalMeasurements(longArg(m, i++));
        s.setMinValue(doubleArg(m, i++));
        s.setMaxValue(doubleArg(m, i++));
        s.setMean(doubleArg(m, i++));
        s.setVariance(doubleArg(m, i));

        return handleSample(s);
    }
}
