package net.fortytwo.extendo.monitron.events;

import net.fortytwo.extendo.monitron.EventHandler;
import net.fortytwo.extendo.monitron.data.Data;
import net.fortytwo.extendo.ontologies.EventOntology;
import net.fortytwo.extendo.ontologies.OMOntology;
import net.fortytwo.extendo.ontologies.OWLTime;
import net.fortytwo.extendo.ontologies.Universe;
import org.openrdf.model.Resource;
import org.openrdf.model.URI;

import java.util.Date;

/**
 * @author Joshua Shinavier (http://fortytwo.net)
 */
public abstract class Observation extends Event {
    protected final URI event;
    protected final Resource startTime;
    protected final Resource endTime;
    protected final URI sensor;

    public Observation(final EventHandler context,
                       final URI sensor,
                       final Data data) {
        super(context);

        event = coinEventURI();
        addStatement(d, event, EventOntology.PLACE, Universe.ROOM_1);  // TODO: don't hard-code location

        this.sensor = sensor;
        addStatement(d, event, OMOntology.PROCEDURE, sensor);

        startTime = vf.createBNode();
        endTime = vf.createBNode();
        addStatement(d, startTime, OWLTime.IN_XSD_DATE_TIME, toLiteral(new Date(data.getStartTime())));
        addStatement(d, endTime, OWLTime.IN_XSD_DATE_TIME, toLiteral(new Date(data.getEndTime())));
    }
}
