package net.fortytwo.extendo.monitron.listeners.sensors;

import net.fortytwo.extendo.monitron.Context;
import net.fortytwo.extendo.monitron.data.GaussianData;
import net.fortytwo.extendo.monitron.events.Event;
import net.fortytwo.extendo.monitron.events.VibrationLevelObservation;
import org.openrdf.model.URI;

/**
 * @author Joshua Shinavier (http://fortytwo.net)
 */
public class VibrationLevelSensorListener extends GaussianSensorListener {
    public VibrationLevelSensorListener(final Context context,
                                        final URI sensor) {
        super(context, sensor);
    }

    protected Event handleSample(final GaussianData data) {
        return new VibrationLevelObservation(context, sensor, data);
    }
}
