package net.fortytwo.extendo.monitron.events;

import net.fortytwo.extendo.monitron.MonitronEventHandler;
import net.fortytwo.extendo.monitron.data.Data;
import net.fortytwo.extendo.ontologies.OMOntology;
import net.fortytwo.extendo.ontologies.OWLTime;
import net.fortytwo.extendo.ontologies.Universe;
import org.openrdf.model.Resource;
import org.openrdf.model.URI;
import org.openrdf.model.vocabulary.RDF;

import java.util.Date;

/**
 * @author Joshua Shinavier (http://fortytwo.net)
 */
public abstract class Observation extends Event {
    protected final URI event;
    protected final Resource samplingTime;
    protected final Resource beginning;
    protected final Resource end;
    protected final URI sensor;
    protected final Resource result;

    public Observation(final MonitronEventHandler context,
                       final URI sensor,
                       final Data data) {
        super(context);
        this.sensor = sensor;

        event = coinEventURI();
        // add event type and observed property in subclasses

        // TODO: don't hard-code location
        addStatement(d, event, OMOntology.OBSERVATION_LOCATION, Universe.ROOM_1);
        //addStatement(d, event, EventOntology.PLACE, Universe.ROOM_1);
        addStatement(d, event, OMOntology.FEATURE_OF_INTEREST, Universe.ROOM_1);

        addStatement(d, event, OMOntology.PROCEDURE, sensor);

        samplingTime = vf.createBNode();
        addStatement(d, samplingTime, RDF.TYPE, OWLTime.INTERVAL);
        addStatement(d, event, OMOntology.SAMPLING_TIME, samplingTime);
        beginning = vf.createBNode();
        end = vf.createBNode();
        addStatement(d, beginning, OWLTime.IN_XSD_DATE_TIME, toLiteral(new Date(data.getSampleIntervalBeginning())));
        addStatement(d, end, OWLTime.IN_XSD_DATE_TIME, toLiteral(new Date(data.getSampleIntervalEnd())));
        addStatement(d, samplingTime, OWLTime.HAS_BEGINNING, beginning);
        addStatement(d, samplingTime, OWLTime.HAS_END, end);

        result = vf.createBNode();
        addStatement(d, result, RDF.TYPE, OMOntology.RESULT_DATA);
        addStatement(d, event, OMOntology.RESULT, result);
        // add details of result in subclasses
    }
}
