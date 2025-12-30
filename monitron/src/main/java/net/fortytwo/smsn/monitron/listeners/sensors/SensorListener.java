package net.fortytwo.smsn.monitron.listeners.sensors;

import net.fortytwo.smsn.monitron.Context;
import net.fortytwo.smsn.monitron.listeners.MonitronListener;
import org.eclipse.rdf4j.model.IRI;

public abstract class SensorListener extends MonitronListener {
    protected final IRI sensor;

    protected SensorListener(final Context context,
                             final IRI sensor) {
        super(context);
        this.sensor = sensor;
    }
}
