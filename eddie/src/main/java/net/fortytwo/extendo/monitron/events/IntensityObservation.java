package net.fortytwo.extendo.monitron.events;

import net.fortytwo.extendo.monitron.EventHandler;
import net.fortytwo.extendo.monitron.data.IntensityData;
import org.openrdf.model.URI;

/**
 * @author Joshua Shinavier (http://fortytwo.net)
 */
public class IntensityObservation extends Observation {

    public IntensityObservation(final EventHandler context,
                                final URI sensor,
                                final IntensityData data) {
        super(context, sensor, data);

        ...
    }
}
