package net.fortytwo.smsn.monitron.events;

import net.fortytwo.smsn.monitron.Context;
import net.fortytwo.smsn.monitron.data.GaussianData;
import net.fortytwo.smsn.monitron.ontologies.MonitronOntology;
import net.fortytwo.smsn.monitron.ontologies.OMOntology;
import net.fortytwo.smsn.rdf.RDFDataset;
import org.eclipse.rdf4j.model.IRI;
import org.eclipse.rdf4j.model.Literal;
import org.eclipse.rdf4j.model.vocabulary.RDF;

public class LightLevelObservation extends Observation {
    public LightLevelObservation(final Context context,
                                 final IRI sensor,
                                 final GaussianData data) {
        super(context, sensor, data);
    }

    @Override
    public RDFDataset toRDF() {
        

        addStatement(event, RDF.TYPE, MonitronOntology.LIGHT_LEVEL_OBSERVATION);
        addStatement(event, OMOntology.OBSERVED_PROPERTY, MonitronOntology.LIGHT_LEVEL);

        Literal value = valueFactory.createLiteral(((GaussianData) data).getMean());
        addStatement(result, OMOntology.AMOUNT, value);
        // TODO: add units

        return super.toRDF();
    }
}
