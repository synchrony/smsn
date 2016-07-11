package net.fortytwo.smsn.monitron.ontologies;

import net.fortytwo.rdfagents.RDFAgents;
import org.openrdf.model.IRI;

/**
 * @author Joshua Shinavier (http://fortytwo.net)
 */
public interface Universe {

    public static final String NAMESPACE = "http://fortytwo.net/2012/08/universe#";

    // environment
    public static final IRI
            BUILDING_1 = RDFAgents.createIRI(NAMESPACE + "winslowBuilding"),
            APARTMENT_1 = RDFAgents.createIRI(NAMESPACE + "apartment_1"),
            ROOM_1 = RDFAgents.createIRI(NAMESPACE + "room_1");

    // agents
    public static final IRI
            MONITRON_1 = RDFAgents.createIRI(NAMESPACE + "monitron_1"),
            EDDIE_4 = RDFAgents.createIRI(NAMESPACE + "eddie_4");

    // sensors
    public static final IRI
            BOSCH_BMP085_1_BAROMETER = RDFAgents.createIRI(NAMESPACE + "bosch-bmp085_1_barometer"),
            AVAGO_ADJD_S311_CR999_1 = RDFAgents.createIRI(NAMESPACE + "avago-adjd-s311-cr999_1"),
            MAXDETECT_RHT03_1_HYGROMETER = RDFAgents.createIRI(NAMESPACE + "maxdetect-rht03_1_hygrometer"),
            GENERIC_PHOTORESISTOR_1 = RDFAgents.createIRI(NAMESPACE + "generic-photoresistor_1"),
            SHARP_GP2Y101AU0F_1 = RDFAgents.createIRI(NAMESPACE + "sharp-gp2y1010au0f_1"),
            HANSE_SE10_1 = RDFAgents.createIRI(NAMESPACE + "hanse-se10_1"),
            KNOWLES_MD9745APZ_F_1 = RDFAgents.createIRI(NAMESPACE + "knowles-md9745apz-f_1"),
            BOSCH_BMP085_1_THERMOMETER = RDFAgents.createIRI(NAMESPACE + "bosch-bmp085_1_thermometer"),
            MAXDETECT_RHT03_1_THERMOMETER = RDFAgents.createIRI(NAMESPACE + "maxdetect-rht03_1_thermometer"),
            MURATA_7BB_20_6L0_1 = RDFAgents.createIRI(NAMESPACE + "murata-7bb-20-6l0_1");

    public static final String
            OM_SENSOR_7BB206L0_VIBRN = "/om/sensor/7bb206l0/vibr",
            OM_SENSOR_ADJDS311CR999_BLUE = "/om/sensor/adjds311cr999/blue",
            OM_SENSOR_ADJDS311CR999_GREEN = "/om/sensor/adjds311cr999/green",
            OM_SENSOR_ADJDS311CR999_RED = "/om/sensor/adjds311cr999/red",
            OM_SENSOR_BMP085_PRESSURE = "/om/sensor/bmp085/press",
            OM_SENSOR_BMP085_TEMP = "/om/sensor/bmp085/temp",
            OM_SENSOR_GP2Y1010AU0F_DUST = "/om/sensor/gp2y1010au0f/dust",
            OM_SENSOR_MD9745APZF_SOUND = "/om/sensor/md9745apzf/sound",
            OM_SENSOR_PHOTO_LIGHT = "/om/sensor/photo/light",
            OM_SENSOR_RHT03_ERROR = "/om/sensor/rht03/error",
            OM_SENSOR_RHT03_HUMID = "/om/sensor/rht03/humid",
            OM_SENSOR_RHT03_TEMP = "/om/sensor/rht03/temp",
            OM_SENSOR_SE10_MOTION = "/om/sensor/se10/motion",
            OM_SYSTEM_ERROR = "/om/system/error",
            OM_SYSTEM_TIME = "/om/system/time";
}
