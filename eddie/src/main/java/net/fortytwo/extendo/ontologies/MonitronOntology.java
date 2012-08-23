package net.fortytwo.extendo.ontologies;

import org.openrdf.model.URI;
import org.openrdf.model.impl.URIImpl;

/**
 * @author Joshua Shinavier (http://fortytwo.net)
 */
public interface MonitronOntology {

    public static final String NAMESPACE = "http://fortytwo.net/2012/08/monitron#";

    public static final URI
            AIR_TEMPERATURE_OBSERVATION = new URIImpl(NAMESPACE + "AirTemperatureObservation"),
            ATMOSPHERIC_PRESSURE_OBSERVATION = new URIImpl(NAMESPACE + "AtmosphericPressureObservation"),
            COLOR_LIGHT_LEVEL_OBSERVATION = new URIImpl(NAMESPACE + "ColorLightLevelObservation"),
            DUST_LEVEL_OBSERVATION = new URIImpl(NAMESPACE + "DustLevelObservation"),
            LIGHT_LEVEL_OBSERVATION = new URIImpl(NAMESPACE + "LightLevelObservation"),
            MOTION_OBSERVATION = new URIImpl(NAMESPACE + "MotionObservation"),
            RELATIVE_HUMIDITY_OBSERVATION = new URIImpl(NAMESPACE + "RelativeHumidityObservation"),
            SOUND_LEVEL_OBSERVATION = new URIImpl(NAMESPACE + "SoundLevelObservation"),
            VIBRATION_LEVEL_OBSERVATION = new URIImpl(NAMESPACE + "VibrationLevelObservation");
}
