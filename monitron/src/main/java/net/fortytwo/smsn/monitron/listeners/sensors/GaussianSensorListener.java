package net.fortytwo.smsn.monitron.listeners.sensors;

import com.illposed.osc.OSCMessage;
import net.fortytwo.smsn.monitron.Context;
import net.fortytwo.smsn.monitron.data.GaussianData;
import net.fortytwo.smsn.monitron.events.MonitronEvent;
import org.openrdf.model.IRI;

public abstract class GaussianSensorListener extends SensorListener {

    protected GaussianSensorListener(final Context context,
                                     final IRI sensor) {
        super(context, sensor);
    }

    protected abstract MonitronEvent handleSample(GaussianData s);

    protected MonitronEvent handleMessage(final OSCMessage m) throws MessageParseException {

        GaussianData s = new GaussianData();

        int i = 0;

        s.setSampleIntervalBeginning(timeArg(m, i++));
        s.setSampleIntervalEnd(timeArg(m, i++));
        s.setTotalMeasurements(longArg(m, i++));
        s.setMinValue(doubleArg(m, i++));
        s.setMean(doubleArg(m, i++));
        s.setMaxValue(doubleArg(m, i++));
        s.setStandardDeviation(doubleArg(m, i));

        return handleSample(s);
    }
}
