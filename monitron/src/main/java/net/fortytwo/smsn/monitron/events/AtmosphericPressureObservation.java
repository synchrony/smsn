package net.fortytwo.smsn.monitron.events;

import net.fortytwo.smsn.rdf.RDFDataset;
import net.fortytwo.smsn.monitron.Context;
import net.fortytwo.smsn.monitron.data.GaussianData;
import net.fortytwo.smsn.monitron.ontologies.MonitronOntology;
import net.fortytwo.smsn.monitron.ontologies.OMOntology;
import org.eclipse.rdf4j.model.IRI;
import org.eclipse.rdf4j.model.Literal;
import org.eclipse.rdf4j.model.vocabulary.RDF;

public class AtmosphericPressureObservation extends Observation {

    public AtmosphericPressureObservation(final Context context,
                                          final IRI sensor,
                                          final GaussianData data) {
        super(context, sensor, data);
    }

    @Override
    public RDFDataset toRDF() {
        

        addStatement(event, RDF.TYPE, MonitronOntology.ATMOSPHERIC_PRESSURE_OBSERVATION);
        addStatement(event, OMOntology.OBSERVED_PROPERTY, MonitronOntology.ATMOSPHERIC_PRESSURE);

        Literal value = valueFactory.createLiteral(((GaussianData) data).getMean());
        addStatement(result, OMOntology.AMOUNT, value);
        addStatement(result, OMOntology.UOM, MonitronOntology.PASCALS);

        return super.toRDF();
    }
}
