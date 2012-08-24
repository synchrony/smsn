package net.fortytwo.extendo.monitron.listeners.sensors;

import net.fortytwo.extendo.monitron.MonitronEventHandler;
import net.fortytwo.extendo.monitron.data.GaussianData;
import net.fortytwo.extendo.monitron.events.Event;
import net.fortytwo.extendo.monitron.events.LightLevelObservation;
import org.openrdf.model.URI;

/**
 * @author Joshua Shinavier (http://fortytwo.net)
 */
public class LightLevelSensorListener extends GaussianSensorListener {

    public LightLevelSensorListener(final MonitronEventHandler context,
                                       final URI sensor) {
        super(context, sensor);
    }

    protected Event handleSample(final GaussianData data) {
        return new LightLevelObservation(context, sensor, data);
    }
}
