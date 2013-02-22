package net.fortytwo.extendo.monitron.events;

import net.fortytwo.extendo.monitron.Context;
import net.fortytwo.extendo.monitron.data.GaussianData;
import net.fortytwo.extendo.monitron.ontologies.MonitronOntology;
import net.fortytwo.extendo.monitron.ontologies.OMOntology;
import org.openrdf.model.Literal;
import org.openrdf.model.URI;
import org.openrdf.model.vocabulary.RDF;

/**
 * @author Joshua Shinavier (http://fortytwo.net)
 */
public class AtmosphericPressureObservation extends Observation {

    public AtmosphericPressureObservation(final Context context,
                                          final URI sensor,
                                          final GaussianData data) {
        super(context, sensor, data);

        addStatement(d, event, RDF.TYPE, MonitronOntology.ATMOSPHERIC_PRESSURE_OBSERVATION);
        addStatement(d, event, OMOntology.OBSERVED_PROPERTY, MonitronOntology.ATMOSPHERIC_PRESSURE);

        Literal value = vf.createLiteral(data.getMean());
        addStatement(d, result, OMOntology.VALUE, value);
        addStatement(d, result, OMOntology.UOM, MonitronOntology.PASCALS);
    }
}
