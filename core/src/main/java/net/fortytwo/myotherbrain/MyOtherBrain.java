package net.fortytwo.myotherbrain;


import net.fortytwo.myotherbrain.util.properties.PropertyException;
import net.fortytwo.myotherbrain.util.properties.TypedProperties;

import javax.xml.namespace.QName;
import java.io.IOException;
import java.net.URI;
import java.net.URISyntaxException;
import java.security.MessageDigest;
import java.security.NoSuchAlgorithmException;
import java.util.Properties;
import java.util.Random;
import java.util.logging.Logger;

/**
 * Created by IntelliJ IDEA.
 * User: josh
 * Date: Dec 12, 2008
 * Time: 4:04:20 PM
 * To change this template use File | Settings | File Templates.
 */
public class MyOtherBrain {
    public static final String DEFAULT_BASEURI = "http://example.org/replaceThisBaseURI#";
    public static final String MOB_ONTOLOGY_FILE = "myotherbrain.owl";

    // Configuration properties.
    public static final String
            NATIVESTORE_DIRECTORY = "net.fortytwo.myotherbrain.nativeStoreDirectory",
            NEOSAIL_DIRECTORY = "net.fortytwo.myotherbrain.neoSailDirectory",
            NEOFULLTEXT_DIRECTORY = "net.fortytwo.myotherbrain.neoFullTextDirectory",
            RMISAILCLIENT_URI = "net.fortytwo.myotherbrain.rmiSailClientURI",
            SAIL_CLASS = "net.fortytwo.myotherbrain.sailClass",
            NAME = "net.fortytwo.myotherbrain.name",
            VERSION = "net.fortytwo.myotherbrain.version",
            REVISION = "net.fortytwo.myotherbrain.revision";

    public static final String
            CREATED = "created",
            FROM = "from",
            KEY = "key",
            //MEMBER = "member",
            TEXT = "text",
            TO = "to",
            TYPE = "type",
            WEIGHT = "weight";

    /*public static final URI[] LEGAL_DATATYPE_URIS = {
            XMLSchema.STRING
    };*/

    private static final String CONFIG_PROPERTIES_FILE = "myotherbrain.properties";
    private static final String VERSION_PROPERTIES_FILE = "version.properties";
    private static final String LOGGING_PROPERTIES_FILE = "log4j.properties";
    private static final TypedProperties VERSION_PROPERTIES;
    private static final Logger LOGGER;

    private static TypedProperties CONFIGURATION;

    static {
        LOGGER = getLogger(MyOtherBrain.class);

        CONFIGURATION = new TypedProperties();
        VERSION_PROPERTIES = new TypedProperties();
        try {
            CONFIGURATION.load(MyOtherBrain.class.getResourceAsStream(CONFIG_PROPERTIES_FILE));
            VERSION_PROPERTIES.load(MyOtherBrain.class.getResourceAsStream(VERSION_PROPERTIES_FILE));
        } catch (IOException e) {
            throw new ExceptionInInitializerError(e);
        }
    }

    private static final MessageDigest SHA1_DIGEST;

    static {
        try {
            SHA1_DIGEST = MessageDigest.getInstance("SHA1");
        } catch (NoSuchAlgorithmException e) {
            throw new ExceptionInInitializerError(e);
        }
    }

    public static Logger getLogger(final Class c) {
        return Logger.getLogger(c.getName());
    }

    public static TypedProperties getConfiguration() {
        return CONFIGURATION;
    }

    public static void setConfiguration(final Properties properties) {
        CONFIGURATION = new TypedProperties(properties);
    }

    public static String getVersionInfo() {
        String name;
        String version;
        int revision;

        try {
            name = VERSION_PROPERTIES.getString(NAME);
            version = VERSION_PROPERTIES.getString(VERSION);
            revision = VERSION_PROPERTIES.getInt(REVISION);
        } catch (PropertyException e) {
            throw new IllegalStateException(e);
        }

        return name + " " + version + ", revision #" + revision;
    }

    ////////////////////////////////////////////////////////////////////////////
    // temporary stuff /////////////////////////////////////////////////////////

    public static URI toURI(final QName q) {
        try {
            return new URI(q.getNamespaceURI() + q.getLocalPart());
        } catch (URISyntaxException e) {
            throw new IllegalArgumentException(e);
        }
    }

    // TODO: make this configurable
    public static final String
            BASE_URI = "http://myotherbrain.fortytwo.net/",
            ATOM_BASEURI = BASE_URI + "atom/",
            GRAPH_BASEURI = BASE_URI + "graph/";

    private static final Random random = new Random();

    // TODO: move me
    public static String randomAtomIdentifier() {
        // TODO: improve me
        return ATOM_BASEURI + random.nextInt(100000);
    }

    // TODO: move me
    public static String randomGraphIdentifier() {
        // TODO: improve me
        return GRAPH_BASEURI + random.nextInt(100000);
    }

    public static String sha1SumOf(final String key) {
        SHA1_DIGEST.update(key.getBytes());
        String hash = "";
        byte[] digest = SHA1_DIGEST.digest();
        for (byte b : digest) {
            String hex = Integer.toHexString(b);
            if (hex.length() == 1)
                hex = "0" + hex;
            hex = hex.substring(hex.length() - 2);
            hash = hash + hex;
        }
        return hash;
    }
}
