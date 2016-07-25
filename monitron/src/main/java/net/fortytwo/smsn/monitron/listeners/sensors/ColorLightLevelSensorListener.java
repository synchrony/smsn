package net.fortytwo.smsn.monitron.listeners.sensors;

import net.fortytwo.smsn.monitron.Context;
import net.fortytwo.smsn.monitron.data.GaussianData;
import net.fortytwo.smsn.monitron.events.ColorLightLevelObservation;
import net.fortytwo.smsn.monitron.events.MonitronEvent;
import org.openrdf.model.IRI;

/**
 * @author Joshua Shinavier (http://fortytwo.net)
 */
public class ColorLightLevelSensorListener extends GaussianSensorListener {

    private final IRI colorProperty;

    public ColorLightLevelSensorListener(final Context context,
                                         final IRI sensor,
                                         final IRI colorProperty) {
        super(context, sensor);
        this.colorProperty = colorProperty;
    }

    protected MonitronEvent handleSample(final GaussianData data) {
        return new ColorLightLevelObservation(context, sensor, data, colorProperty);
    }
}
