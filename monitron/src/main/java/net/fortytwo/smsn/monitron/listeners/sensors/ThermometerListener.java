package net.fortytwo.smsn.monitron.listeners.sensors;

import net.fortytwo.smsn.monitron.Context;
import net.fortytwo.smsn.monitron.data.GaussianData;
import net.fortytwo.smsn.monitron.events.AirTemperatureObservation;
import net.fortytwo.smsn.monitron.events.MonitronEvent;
import org.eclipse.rdf4j.model.IRI;

public class ThermometerListener extends GaussianSensorListener {
    public ThermometerListener(final Context context,
                               final IRI sensor) {
        super(context, sensor);
    }

    protected MonitronEvent handleSample(final GaussianData data) {
        return new AirTemperatureObservation(context, sensor, data);
    }
}
