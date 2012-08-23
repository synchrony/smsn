package net.fortytwo.extendo.monitron.listeners.sensors;

import net.fortytwo.extendo.monitron.EventHandler;
import net.fortytwo.extendo.monitron.events.Event;
import net.fortytwo.extendo.monitron.events.LightLevelObservation;
import net.fortytwo.extendo.monitron.data.AnalogData;
import net.fortytwo.extendo.monitron.data.IntensityData;
import org.openrdf.model.URI;

/**
 * @author Joshua Shinavier (http://fortytwo.net)
 */
public class LightLevelSensorListener extends AnalogSensorListener {

    protected LightLevelSensorListener(final EventHandler context,
                                       final URI sensor) {
        super(context, sensor);
    }

    protected Event handleSample(AnalogData as) {
        IntensityData is = new IntensityData();

        is.setOverallIntensity(as.getMean());
        is.setMinIntensity(as.getMinValue());
        is.setMaxIntensity(as.getMaxValue());

        return handleSample(is);
    }

    protected Event handleSample(IntensityData s) {
        return new LightLevelObservation(s, )
    }
}
