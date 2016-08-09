package net.fortytwo.smsn.monitron.listeners.sensors;

import net.fortytwo.smsn.monitron.Context;
import net.fortytwo.smsn.monitron.data.GaussianData;
import net.fortytwo.smsn.monitron.events.LightLevelObservation;
import net.fortytwo.smsn.monitron.events.MonitronEvent;
import org.openrdf.model.IRI;

public class LightLevelSensorListener extends GaussianSensorListener {

    public LightLevelSensorListener(final Context context,
                                       final IRI sensor) {
        super(context, sensor);
    }

    protected MonitronEvent handleSample(final GaussianData data) {
        return new LightLevelObservation(context, sensor, data);
    }
}
