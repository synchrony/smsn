package net.fortytwo.extendo.monitron.events;

import net.fortytwo.extendo.monitron.MonitronEventHandler;
import net.fortytwo.extendo.monitron.data.BooleanData;
import net.fortytwo.extendo.ontologies.MonitronOntology;
import net.fortytwo.extendo.ontologies.OMOntology;
import org.openrdf.model.Literal;
import org.openrdf.model.URI;
import org.openrdf.model.vocabulary.RDF;

/**
 * @author Joshua Shinavier (http://fortytwo.net)
 */
public class MotionObservation extends Observation {

    public MotionObservation(final MonitronEventHandler context,
                             final URI sensor,
                             final BooleanData data) {
        super(context, sensor, data);

        addStatement(d, event, RDF.TYPE, MonitronOntology.MOTION_OBSERVATION);
        addStatement(d, event, OMOntology.OBSERVED_PROPERTY, MonitronOntology.IS_MOTION);

        Literal value = vf.createLiteral(data.getResult());
        addStatement(d, result, OMOntology.VALUE, value);
        // no units; the result is a true/false value
    }
}
