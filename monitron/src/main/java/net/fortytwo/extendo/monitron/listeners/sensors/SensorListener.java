package net.fortytwo.extendo.monitron.listeners.sensors;

import net.fortytwo.extendo.monitron.Context;
import net.fortytwo.extendo.monitron.listeners.MonitronListener;
import org.openrdf.model.URI;

/**
 * @author Joshua Shinavier (http://fortytwo.net)
 */
public abstract class SensorListener extends MonitronListener {
    protected final URI sensor;

    protected SensorListener(final Context context,
                             final URI sensor) {
        super(context);
        this.sensor = sensor;
    }
}
