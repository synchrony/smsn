package net.fortytwo.smsn.monitron.events;

import net.fortytwo.smsn.monitron.Context;
import net.fortytwo.smsn.monitron.data.GaussianData;
import net.fortytwo.smsn.monitron.ontologies.MonitronOntology;
import net.fortytwo.smsn.monitron.ontologies.OMOntology;
import net.fortytwo.rdfagents.model.Dataset;
import org.openrdf.model.IRI;
import org.openrdf.model.Literal;
import org.openrdf.model.vocabulary.RDF;

public class RelativeHumidityObservation extends Observation {

    public RelativeHumidityObservation(
            final Context context,
            final IRI sensor,
            final GaussianData data) {

        super(context, sensor, data);
    }

    @Override
    public Dataset toRDF() {
        Dataset dataset = super.toRDF();

        addStatement(dataset, event, RDF.TYPE, MonitronOntology.RELATIVE_HUMIDITY_OBSERVATION);
        addStatement(dataset, event, OMOntology.OBSERVED_PROPERTY, MonitronOntology.RELATIVE_HUMIDITY);

        Literal value = valueFactory.createLiteral(((GaussianData) data).getMean());
        addStatement(dataset, result, OMOntology.AMOUNT, value);
        // no units (relative humidity is dimensionless)

        return dataset;
    }
}
