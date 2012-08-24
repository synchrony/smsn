package net.fortytwo.extendo.monitron.listeners.sensors;

import com.illposed.osc.OSCMessage;
import net.fortytwo.extendo.monitron.MonitronEventHandler;
import net.fortytwo.extendo.monitron.data.GaussianData;
import net.fortytwo.extendo.monitron.events.Event;
import org.openrdf.model.URI;

/**
 * @author Joshua Shinavier (http://fortytwo.net)
 */
public abstract class GaussianSensorListener extends SensorListener {

    protected GaussianSensorListener(final MonitronEventHandler context,
                                     final URI sensor) {
        super(context, sensor);
    }

    protected abstract Event handleSample(GaussianData s);

    protected Event handleMessage(final OSCMessage m) throws MessageParseException {

        GaussianData s = new GaussianData();

        int i = 0;

        s.setSampleIntervalBeginning(hexLongArg(m, i++));
        s.setSampleIntervalEnd(hexLongArg(m, i++));
        s.setTotalMeasurements(longArg(m, i++));
        s.setMinValue(doubleArg(m, i++));
        s.setMaxValue(doubleArg(m, i++));
        s.setMean(doubleArg(m, i++));
        s.setVariance(doubleArg(m, i));

        return handleSample(s);
    }
}
