package net.fortytwo.smsn.monitron.events;

import net.fortytwo.smsn.monitron.Context;
import net.fortytwo.smsn.monitron.data.GaussianData;
import net.fortytwo.smsn.monitron.ontologies.MonitronOntology;
import net.fortytwo.smsn.monitron.ontologies.OMOntology;
import net.fortytwo.rdfagents.model.Dataset;
import org.openrdf.model.Literal;
import org.openrdf.model.URI;
import org.openrdf.model.vocabulary.RDF;

/**
 * @author Joshua Shinavier (http://fortytwo.net)
 */
public class ColorLightLevelObservation extends Observation {

    protected final URI colorProperty;

    public ColorLightLevelObservation(final Context context,
                                      final URI sensor,
                                      final GaussianData data,
                                      final URI colorProperty) {
        super(context, sensor, data);

        this.colorProperty = colorProperty;
    }

    public URI getColorProperty() {
        return colorProperty;
    }

    @Override
    public Dataset toRDF() {
        Dataset dataset = super.toRDF();

        addStatement(dataset, event, RDF.TYPE, MonitronOntology.COLOR_LIGHT_LEVEL_OBSERVATION);

        addStatement(dataset, event, OMOntology.OBSERVED_PROPERTY, colorProperty);

        Literal value = valueFactory.createLiteral(((GaussianData) data).getMean());
        addStatement(dataset, result, OMOntology.VALUE, value);
        // TODO: add units

        return dataset;
    }
}
