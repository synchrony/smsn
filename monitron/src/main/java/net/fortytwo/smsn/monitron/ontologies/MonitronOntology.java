package net.fortytwo.smsn.monitron.ontologies;

import net.fortytwo.rdfagents.RDFAgents;
import org.openrdf.model.IRI;

public interface MonitronOntology {

    String NAMESPACE = "http://fortytwo.net/2012/08/monitron#";

    // classes
    IRI
            AIR_TEMPERATURE_OBSERVATION = RDFAgents.createIRI(NAMESPACE + "AirTemperatureObservation"),
            ATMOSPHERIC_PRESSURE_OBSERVATION = RDFAgents.createIRI(NAMESPACE + "AtmosphericPressureObservation"),
            COLOR = RDFAgents.createIRI(NAMESPACE + "Color"),
            COLOR_LIGHT_LEVEL_OBSERVATION = RDFAgents.createIRI(NAMESPACE + "ColorLightLevelObservation"),
            DUST_LEVEL_OBSERVATION = RDFAgents.createIRI(NAMESPACE + "DustLevelObservation"),
            LIGHT_LEVEL_OBSERVATION = RDFAgents.createIRI(NAMESPACE + "LightLevelObservation"),
            MOTION_OBSERVATION = RDFAgents.createIRI(NAMESPACE + "MotionObservation"),
            RELATIVE_HUMIDITY_OBSERVATION = RDFAgents.createIRI(NAMESPACE + "RelativeHumidityObservation"),
            SOUND_LEVEL_OBSERVATION = RDFAgents.createIRI(NAMESPACE + "SoundLevelObservation"),
            VIBRATION_LEVEL_OBSERVATION = RDFAgents.createIRI(NAMESPACE + "VibrationLevelObservation");

    // UnitOfMeasurement individuals
    IRI
            DEGREES_CELSIUS = RDFAgents.createIRI(NAMESPACE + "degreesCelsius"),
            PASCALS = RDFAgents.createIRI(NAMESPACE + "pascals");

    // Property individuals
    IRI
            AIR_TEMPERATURE = RDFAgents.createIRI(NAMESPACE + "airTemperature"),
            ATMOSPHERIC_PRESSURE = RDFAgents.createIRI(NAMESPACE + "atmosphericPressure"),
            BLUE_LIGHT_LEVEL = RDFAgents.createIRI(NAMESPACE + "blueLightLevel"),
            GREEN_LIGHT_LEVEL = RDFAgents.createIRI(NAMESPACE + "greenLightLevel"),
            RED_LIGHT_LEVEL = RDFAgents.createIRI(NAMESPACE + "redLightLevel"),
            IS_MOTION = RDFAgents.createIRI(NAMESPACE + "isMotion"),
            LIGHT_LEVEL = RDFAgents.createIRI(NAMESPACE + "lightLevel"),
            OPTICAL_DUST_LEVEL = RDFAgents.createIRI(NAMESPACE + "opticalDustLevel"),
            RELATIVE_HUMIDITY = RDFAgents.createIRI(NAMESPACE + "relativeHumidity"),
            SOUND_LEVEL = RDFAgents.createIRI(NAMESPACE + "soundLevel"),
            VIBRATION_LEVEL = RDFAgents.createIRI(NAMESPACE + "vibrationLevel");
}
