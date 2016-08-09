package net.fortytwo.smsn.monitron.events;

import net.fortytwo.rdfagents.model.Dataset;
import net.fortytwo.smsn.monitron.Context;
import net.fortytwo.smsn.monitron.data.GaussianData;
import net.fortytwo.smsn.monitron.ontologies.MonitronOntology;
import net.fortytwo.smsn.monitron.ontologies.OMOntology;
import org.openrdf.model.IRI;
import org.openrdf.model.Literal;
import org.openrdf.model.vocabulary.RDF;

public class ColorLightLevelObservation extends Observation {

    private final IRI colorProperty;

    public ColorLightLevelObservation(final Context context,
                                      final IRI sensor,
                                      final GaussianData data,
                                      final IRI colorProperty) {
        super(context, sensor, data);

        this.colorProperty = colorProperty;
    }

    public IRI getColorProperty() {
        return colorProperty;
    }

    @Override
    public Dataset toRDF() {
        Dataset dataset = super.toRDF();

        addStatement(dataset, event, RDF.TYPE, MonitronOntology.COLOR_LIGHT_LEVEL_OBSERVATION);

        addStatement(dataset, event, OMOntology.OBSERVED_PROPERTY, colorProperty);

        Literal value = valueFactory.createLiteral(((GaussianData) data).getMean());
        addStatement(dataset, result, OMOntology.AMOUNT, value);
        // TODO: add units

        return dataset;
    }
}
