package net.fortytwo.extendo.monitron.events;

import net.fortytwo.extendo.monitron.EventHandler;
import net.fortytwo.extendo.monitron.data.IntensityData;

/**
 * @author Joshua Shinavier (http://fortytwo.net)
 */
public class SoundLevelObservation extends Event {
    public SoundLevelObservation(final EventHandler context,
                                 final IntensityData s) {
        super(context);


    }
}
