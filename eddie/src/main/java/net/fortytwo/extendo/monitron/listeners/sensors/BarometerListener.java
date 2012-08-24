package net.fortytwo.extendo.monitron.listeners.sensors;

import net.fortytwo.extendo.monitron.MonitronEventHandler;
import net.fortytwo.extendo.monitron.data.GaussianData;
import net.fortytwo.extendo.monitron.events.AtmosphericPressureObservation;
import net.fortytwo.extendo.monitron.events.Event;
import org.openrdf.model.URI;

/**
 * @author Joshua Shinavier (http://fortytwo.net)
 */
public class BarometerListener extends GaussianSensorListener {
    public BarometerListener(final MonitronEventHandler context, final URI sensor) {
        super(context, sensor);
    }

    protected Event handleSample(final GaussianData data) {
        return new AtmosphericPressureObservation(context, sensor, data);
    }
}
