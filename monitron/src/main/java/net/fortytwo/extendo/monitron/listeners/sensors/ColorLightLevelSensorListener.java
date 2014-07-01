package net.fortytwo.extendo.monitron.listeners.sensors;

import net.fortytwo.extendo.monitron.Context;
import net.fortytwo.extendo.monitron.data.GaussianData;
import net.fortytwo.extendo.monitron.events.ColorLightLevelObservation;
import net.fortytwo.extendo.monitron.events.MonitronEvent;
import org.openrdf.model.URI;

/**
 * @author Joshua Shinavier (http://fortytwo.net)
 */
public class ColorLightLevelSensorListener extends GaussianSensorListener {

    protected final URI colorProperty;

    public ColorLightLevelSensorListener(final Context context,
                                         final URI sensor,
                                         final URI colorProperty) {
        super(context, sensor);
        this.colorProperty = colorProperty;
    }

    protected MonitronEvent handleSample(final GaussianData data) {
        return new ColorLightLevelObservation(context, sensor, data, colorProperty);
    }
}
