package net.fortytwo.smsn.monitron.events;

import net.fortytwo.smsn.rdf.RDFDataset;
import net.fortytwo.smsn.monitron.Context;
import net.fortytwo.smsn.monitron.data.BooleanData;
import net.fortytwo.smsn.monitron.ontologies.MonitronOntology;
import net.fortytwo.smsn.monitron.ontologies.OMOntology;
import org.eclipse.rdf4j.model.IRI;
import org.eclipse.rdf4j.model.Literal;
import org.eclipse.rdf4j.model.vocabulary.RDF;

public class MotionObservation extends Observation {

    public MotionObservation(final Context context,
                             final IRI sensor,
                             final BooleanData data) {
        super(context, sensor, data);
    }

    @Override
    public RDFDataset toRDF() {
        

        addStatement(event, RDF.TYPE, MonitronOntology.MOTION_OBSERVATION);
        addStatement(event, OMOntology.OBSERVED_PROPERTY, MonitronOntology.IS_MOTION);

        Literal value = valueFactory.createLiteral(((BooleanData) data).getResult());
        addStatement(result, OMOntology.AMOUNT, value);
        // no units; the result is a true/false value

        return super.toRDF();
    }
}
