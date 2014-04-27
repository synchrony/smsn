package net.fortytwo.extendo.p2p;

import org.json.JSONException;
import org.json.JSONObject;

/**
 * @author Joshua Shinavier (http://fortytwo.net)
 */
public class ServiceDescription {

    // see also corresponding Extendo configuration properties
    private static final String
            PROP_ENDPOINT = "endpoint",
            PROP_OSC_PORT = "oscPort",
            PROP_PUBSUB_PORT = "pubsubPort",
            PROP_VERSION = "version";

    private final String endpoint;
    private final int oscPort;
    private final int pubsubPort;
    private final String version;

    public ServiceDescription(final String version,
                              final String endpoint,
                              final int oscPort,
                              final int pubsubPort) {
        this.endpoint = endpoint;
        this.oscPort = oscPort;
        this.pubsubPort = pubsubPort;
        this.version = version;
    }

    public ServiceDescription(final String jsonStr) throws InvalidServiceDescriptionException {
        try {
            JSONObject j = new JSONObject(jsonStr);

            endpoint = j.getString(PROP_ENDPOINT);
            version = j.getString(PROP_VERSION);
            oscPort = j.getInt(PROP_OSC_PORT);
            pubsubPort = j.getInt(PROP_PUBSUB_PORT);
        } catch (JSONException e) {
            throw new InvalidServiceDescriptionException(e.getMessage());
        } catch (NumberFormatException e) {
            throw new InvalidServiceDescriptionException(e.getMessage());
        }
    }

    public String getEndpoint() {
        return endpoint;
    }

    public int getOscPort() {
        return oscPort;
    }

    public int getPubsubPort() {
        return pubsubPort;
    }

    public String getVersion() {
        return version;
    }

    public JSONObject toJSON() throws JSONException {
        JSONObject j = new JSONObject();
        j.put(PROP_VERSION, version);
        j.put(PROP_ENDPOINT, endpoint);
        j.put(PROP_OSC_PORT, oscPort);
        j.put(PROP_PUBSUB_PORT, pubsubPort);
        return j;
    }

    public class InvalidServiceDescriptionException extends Exception {
        public InvalidServiceDescriptionException(final String message) {
            super(message);
        }
    }
}
