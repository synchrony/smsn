package net.fortytwo.extendo.monitron.events;

import net.fortytwo.extendo.monitron.EventHandler;
import net.fortytwo.extendo.monitron.data.IntensityData;
import net.fortytwo.extendo.ontologies.MonitronOntology;
import org.openrdf.model.URI;
import org.openrdf.model.vocabulary.RDF;

/**
 * @author Joshua Shinavier (http://fortytwo.net)
 */
public class LightLevelObservation extends IntensityObservation {
    public LightLevelObservation(final EventHandler context,
                                 final URI sensor,
                                 final IntensityData data) {
        super(context, sensor, data);

        addStatement(d, event, RDF.TYPE, MonitronOntology.LIGHT_LEVEL_OBSERVATION);
    }
}
