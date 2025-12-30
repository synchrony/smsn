package net.fortytwo.smsn.monitron.ontologies;

import net.fortytwo.smsn.rdf.RDF4JUtil;
import org.eclipse.rdf4j.model.IRI;

public interface MonitronOntology {

    String NAMESPACE = "http://fortytwo.net/2012/08/monitron#";

    // classes
    IRI
            AIR_TEMPERATURE_OBSERVATION = RDF4JUtil.createIRI(NAMESPACE + "AirTemperatureObservation"),
            ATMOSPHERIC_PRESSURE_OBSERVATION = RDF4JUtil.createIRI(NAMESPACE + "AtmosphericPressureObservation"),
            COLOR = RDF4JUtil.createIRI(NAMESPACE + "Color"),
            COLOR_LIGHT_LEVEL_OBSERVATION = RDF4JUtil.createIRI(NAMESPACE + "ColorLightLevelObservation"),
            DUST_LEVEL_OBSERVATION = RDF4JUtil.createIRI(NAMESPACE + "DustLevelObservation"),
            LIGHT_LEVEL_OBSERVATION = RDF4JUtil.createIRI(NAMESPACE + "LightLevelObservation"),
            MOTION_OBSERVATION = RDF4JUtil.createIRI(NAMESPACE + "MotionObservation"),
            RELATIVE_HUMIDITY_OBSERVATION = RDF4JUtil.createIRI(NAMESPACE + "RelativeHumidityObservation"),
            SOUND_LEVEL_OBSERVATION = RDF4JUtil.createIRI(NAMESPACE + "SoundLevelObservation"),
            VIBRATION_LEVEL_OBSERVATION = RDF4JUtil.createIRI(NAMESPACE + "VibrationLevelObservation");

    // UnitOfMeasurement individuals
    IRI
            DEGREES_CELSIUS = RDF4JUtil.createIRI(NAMESPACE + "degreesCelsius"),
            PASCALS = RDF4JUtil.createIRI(NAMESPACE + "pascals");

    // Property individuals
    IRI
            AIR_TEMPERATURE = RDF4JUtil.createIRI(NAMESPACE + "airTemperature"),
            ATMOSPHERIC_PRESSURE = RDF4JUtil.createIRI(NAMESPACE + "atmosphericPressure"),
            BLUE_LIGHT_LEVEL = RDF4JUtil.createIRI(NAMESPACE + "blueLightLevel"),
            GREEN_LIGHT_LEVEL = RDF4JUtil.createIRI(NAMESPACE + "greenLightLevel"),
            RED_LIGHT_LEVEL = RDF4JUtil.createIRI(NAMESPACE + "redLightLevel"),
            IS_MOTION = RDF4JUtil.createIRI(NAMESPACE + "isMotion"),
            LIGHT_LEVEL = RDF4JUtil.createIRI(NAMESPACE + "lightLevel"),
            OPTICAL_DUST_LEVEL = RDF4JUtil.createIRI(NAMESPACE + "opticalDustLevel"),
            RELATIVE_HUMIDITY = RDF4JUtil.createIRI(NAMESPACE + "relativeHumidity"),
            SOUND_LEVEL = RDF4JUtil.createIRI(NAMESPACE + "soundLevel"),
            VIBRATION_LEVEL = RDF4JUtil.createIRI(NAMESPACE + "vibrationLevel");
}
