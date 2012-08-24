package net.fortytwo.extendo.ontologies;

import org.openrdf.model.URI;
import org.openrdf.model.impl.URIImpl;

/**
 * @author Joshua Shinavier (http://fortytwo.net)
 */
public interface MonitronOntology {

    public static final String NAMESPACE = "http://fortytwo.net/2012/08/monitron#";

    // classes
    public static final URI
            AIR_TEMPERATURE_OBSERVATION = new URIImpl(NAMESPACE + "AirTemperatureObservation"),
            ATMOSPHERIC_PRESSURE_OBSERVATION = new URIImpl(NAMESPACE + "AtmosphericPressureObservation"),
            COLOR = new URIImpl(NAMESPACE + "Color"),
            COLOR_LIGHT_LEVEL_OBSERVATION = new URIImpl(NAMESPACE + "ColorLightLevelObservation"),
            DUST_LEVEL_OBSERVATION = new URIImpl(NAMESPACE + "DustLevelObservation"),
            LIGHT_LEVEL_OBSERVATION = new URIImpl(NAMESPACE + "LightLevelObservation"),
            MOTION_OBSERVATION = new URIImpl(NAMESPACE + "MotionObservation"),
            RELATIVE_HUMIDITY_OBSERVATION = new URIImpl(NAMESPACE + "RelativeHumidityObservation"),
            SOUND_LEVEL_OBSERVATION = new URIImpl(NAMESPACE + "SoundLevelObservation"),
            VIBRATION_LEVEL_OBSERVATION = new URIImpl(NAMESPACE + "VibrationLevelObservation");

    // UnitOfMeasurement individuals
    public static final URI
            DEGREES_CELSIUS = new URIImpl(NAMESPACE + "degreesCelsius"),
            PASCALS = new URIImpl(NAMESPACE + "pascals");

    // Property individuals
    public static final URI
            AIR_TEMPERATURE = new URIImpl(NAMESPACE + "airTemperature"),
            ATMOSPHERIC_PRESSURE = new URIImpl(NAMESPACE + "atmosphericPressure"),
            BLUE_LIGHT_LEVEL = new URIImpl(NAMESPACE + "blueLightLevel"),
            GREEN_LIGHT_LEVEL = new URIImpl(NAMESPACE + "greenLightLevel"),
            RED_LIGHT_LEVEL = new URIImpl(NAMESPACE + "redLightLevel"),
            IS_MOTION = new URIImpl(NAMESPACE + "isMotion"),
            LIGHT_LEVEL = new URIImpl(NAMESPACE + "lightLevel"),
            OPTICAL_DUST_LEVEL = new URIImpl(NAMESPACE + "opticalDustLevel"),
            RELATIVE_HUMIDITY = new URIImpl(NAMESPACE + "relativeHumidity"),
            SOUND_LEVEL = new URIImpl(NAMESPACE + "soundLevel"),
            VIBRATION_LEVEL = new URIImpl(NAMESPACE + "vibrationLevel");
}
