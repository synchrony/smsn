package net.fortytwo.smsn.monitron.listeners.sensors;

import net.fortytwo.smsn.monitron.Context;
import net.fortytwo.smsn.monitron.data.GaussianData;
import net.fortytwo.smsn.monitron.events.DustLevelObservation;
import net.fortytwo.smsn.monitron.events.MonitronEvent;
import org.openrdf.model.URI;

/**
 * @author Joshua Shinavier (http://fortytwo.net)
 */
public class OpticalDustSensorListener extends GaussianSensorListener {

    public OpticalDustSensorListener(final Context context,
                                        final URI sensor) {
        super(context, sensor);
    }

    protected MonitronEvent handleSample(final GaussianData data) {
        return new DustLevelObservation(context, sensor, data);
    }
}
