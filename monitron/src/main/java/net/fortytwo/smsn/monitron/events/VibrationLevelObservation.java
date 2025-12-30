package net.fortytwo.smsn.monitron.events;

import net.fortytwo.smsn.monitron.Context;
import net.fortytwo.smsn.monitron.data.GaussianData;
import net.fortytwo.smsn.monitron.ontologies.MonitronOntology;
import net.fortytwo.smsn.monitron.ontologies.OMOntology;
import net.fortytwo.smsn.rdf.RDFDataset;
import org.eclipse.rdf4j.model.IRI;
import org.eclipse.rdf4j.model.Literal;
import org.eclipse.rdf4j.model.vocabulary.RDF;

public class VibrationLevelObservation extends Observation {

    public VibrationLevelObservation(final Context context,
                                     final IRI sensor,
                                     final GaussianData data) {
        super(context, sensor, data);
    }

    @Override
    public RDFDataset toRDF() {
        

        addStatement(event, RDF.TYPE, MonitronOntology.VIBRATION_LEVEL_OBSERVATION);
        addStatement(event, OMOntology.OBSERVED_PROPERTY, MonitronOntology.VIBRATION_LEVEL);

        Literal value = valueFactory.createLiteral(((GaussianData) data).getMaxValue());
        addStatement(result, OMOntology.AMOUNT, value);
        // TODO: add units

        return super.toRDF();
    }
}
