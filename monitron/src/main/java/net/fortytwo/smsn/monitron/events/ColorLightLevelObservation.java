package net.fortytwo.smsn.monitron.events;

import net.fortytwo.smsn.rdf.RDFDataset;
import net.fortytwo.smsn.monitron.Context;
import net.fortytwo.smsn.monitron.data.GaussianData;
import net.fortytwo.smsn.monitron.ontologies.MonitronOntology;
import net.fortytwo.smsn.monitron.ontologies.OMOntology;
import org.eclipse.rdf4j.model.IRI;
import org.eclipse.rdf4j.model.Literal;
import org.eclipse.rdf4j.model.vocabulary.RDF;

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
    public RDFDataset toRDF() {
        

        addStatement(event, RDF.TYPE, MonitronOntology.COLOR_LIGHT_LEVEL_OBSERVATION);

        addStatement(event, OMOntology.OBSERVED_PROPERTY, colorProperty);

        Literal value = valueFactory.createLiteral(((GaussianData) data).getMean());
        addStatement(result, OMOntology.AMOUNT, value);
        // TODO: add units

        return super.toRDF();
    }
}
