package net.fortytwo.smsn.monitron.events;

import net.fortytwo.rdfagents.model.Dataset;
import net.fortytwo.smsn.monitron.Context;
import net.fortytwo.smsn.monitron.data.GaussianData;
import net.fortytwo.smsn.monitron.ontologies.MonitronOntology;
import net.fortytwo.smsn.monitron.ontologies.OMOntology;
import org.openrdf.model.IRI;
import org.openrdf.model.Literal;
import org.openrdf.model.vocabulary.RDF;

public class AirTemperatureObservation extends Observation {

    public AirTemperatureObservation(final Context context,
                                     final IRI sensor,
                                     final GaussianData data) {
        super(context, sensor, data);
    }

    @Override
    public Dataset toRDF() {
        Dataset dataset = super.toRDF();

        addStatement(dataset, event, RDF.TYPE, MonitronOntology.AIR_TEMPERATURE_OBSERVATION);
        addStatement(dataset, event, OMOntology.OBSERVED_PROPERTY, MonitronOntology.AIR_TEMPERATURE);

        Literal value = valueFactory.createLiteral(((GaussianData) data).getMean());
        addStatement(dataset, result, OMOntology.AMOUNT, value);
        addStatement(dataset, result, OMOntology.UOM, MonitronOntology.DEGREES_CELSIUS);

        return dataset;
    }
}
