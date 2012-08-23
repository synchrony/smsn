package net.fortytwo.extendo.monitron.events;

import net.fortytwo.extendo.monitron.EventHandler;
import net.fortytwo.extendo.monitron.data.BooleanData;
import net.fortytwo.extendo.ontologies.EventOntology;
import net.fortytwo.extendo.ontologies.OWLTime;
import org.openrdf.model.Resource;
import org.openrdf.model.URI;

/**
 * @author Joshua Shinavier (http://fortytwo.net)
 */
public class MotionObservation extends Observation {

    public MotionObservation(final EventHandler context,
                             final URI sensor,
                             final BooleanData data) {
        super(context, sensor, data);

        Resource instant = vf.createBNode();
        addStatement(d, event, EventOntology.TIME, instant);

        addStatement(d, instant, OWLTime.AFTER, startTime);
        addStatement(d, instant, OWLTime.BEFORE, endTime);

        ...
    }
}
