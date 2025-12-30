package net.fortytwo.smsn.monitron.events;

import net.fortytwo.smsn.monitron.Context;
import net.fortytwo.smsn.monitron.data.Data;
import net.fortytwo.smsn.monitron.ontologies.OMOntology;
import net.fortytwo.smsn.monitron.ontologies.OWLTime;
import net.fortytwo.smsn.monitron.ontologies.Universe;
import net.fortytwo.smsn.rdf.RDFDataset;
import org.eclipse.rdf4j.model.IRI;
import org.eclipse.rdf4j.model.Resource;
import org.eclipse.rdf4j.model.vocabulary.RDF;

import java.util.Date;

public abstract class Observation extends MonitronEvent {

    protected final Data data;
    protected final IRI sensor;
    protected final IRI event;
    protected final Resource result;

    public Observation(final Context context,
                       final IRI sensor,
                       final Data data) {
        super(context);
        this.sensor = sensor;
        event = coinEventIRI();
        this.data = data;
        result = valueFactory.createBNode();
    }

    public IRI getSensor() {
        return sensor;
    }

    public Data getData() {
        return data;
    }

    @Override
    public RDFDataset toRDF() {
        // TODO: use appropriate class for result type in Observation subclasses
        // (om:SimpleMeasure is only for xsd:double values)
        addStatement(result, RDF.TYPE, OMOntology.SIMPLE_MEASURE);
        addStatement(event, OMOntology.RESULT, result);
        // add details of result in subclasses

        // TODO: don't hard-code location
        addStatement(event, OMOntology.FEATURE_OF_INTEREST, Universe.ROOM_1);

        addStatement(event, OMOntology.PROCEDURE, sensor);

        Resource samplingTime = valueFactory.createBNode();
        addStatement(samplingTime, RDF.TYPE, OWLTime.INTERVAL);
        addStatement(event, OMOntology.PHENOMENON_TIME, samplingTime);
        Resource beginning = valueFactory.createBNode();
        addStatement(beginning, RDF.TYPE, OWLTime.INSTANT);
        Resource end = valueFactory.createBNode();
        addStatement(end, RDF.TYPE, OWLTime.INSTANT);
        addStatement(beginning, OWLTime.IN_XSD_DATE_TIME,
                toLiteral(new Date(data.getSampleIntervalBeginning())));
        addStatement(end, OWLTime.IN_XSD_DATE_TIME,
                toLiteral(new Date(data.getSampleIntervalEnd())));
        addStatement(samplingTime, OWLTime.HAS_BEGINNING, beginning);
        addStatement(samplingTime, OWLTime.HAS_END, end);

        return super.toRDF();
    }
}
