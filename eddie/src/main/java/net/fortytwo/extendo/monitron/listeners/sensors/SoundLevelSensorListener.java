package net.fortytwo.extendo.monitron.listeners.sensors;

import net.fortytwo.extendo.monitron.EventHandler;
import net.fortytwo.extendo.monitron.data.AnalogData;

/**
 * @author Joshua Shinavier (http://fortytwo.net)
 */
public class SoundLevelSensorListener extends AnalogSensorListener {

    protected SoundLevelSensorListener(final EventHandler context) {
        super(context);
    }

    protected void handleSample(final AnalogData s) {

    }

}
