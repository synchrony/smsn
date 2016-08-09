package net.fortytwo.smsn.monitron.events;

import net.fortytwo.rdfagents.model.Dataset;
import net.fortytwo.smsn.monitron.Context;
import net.fortytwo.smsn.monitron.data.GaussianData;
import net.fortytwo.smsn.monitron.ontologies.MonitronOntology;
import net.fortytwo.smsn.monitron.ontologies.OMOntology;
import org.openrdf.model.IRI;
import org.openrdf.model.Literal;
import org.openrdf.model.vocabulary.RDF;

public class SoundLevelObservation extends Observation {
    public SoundLevelObservation(final Context context,
                                 final IRI sensor,
                                 final GaussianData data) {
        super(context, sensor, data);
    }

    @Override
    public Dataset toRDF() {
        Dataset dataset = super.toRDF();

        addStatement(dataset, event, RDF.TYPE, MonitronOntology.SOUND_LEVEL_OBSERVATION);
        addStatement(dataset, event, OMOntology.OBSERVED_PROPERTY, MonitronOntology.SOUND_LEVEL);

        Literal value = valueFactory.createLiteral(((GaussianData) data).getMaxValue());
        addStatement(dataset, result, OMOntology.AMOUNT, value);
        // TODO: add units

        return dataset;
    }
}
