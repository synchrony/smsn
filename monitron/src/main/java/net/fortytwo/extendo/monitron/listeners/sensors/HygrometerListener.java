package net.fortytwo.extendo.monitron.listeners.sensors;

import net.fortytwo.extendo.monitron.Context;
import net.fortytwo.extendo.monitron.data.GaussianData;
import net.fortytwo.extendo.monitron.events.MonitronEvent;
import net.fortytwo.extendo.monitron.events.RelativeHumidityObservation;
import org.openrdf.model.URI;

/**
 * @author Joshua Shinavier (http://fortytwo.net)
 */
public class HygrometerListener extends GaussianSensorListener {

    public HygrometerListener(final Context context, final URI sensor) {
        super(context, sensor);
    }

    protected MonitronEvent handleSample(final GaussianData data) {
        return new RelativeHumidityObservation(context, sensor, data);
    }
}
