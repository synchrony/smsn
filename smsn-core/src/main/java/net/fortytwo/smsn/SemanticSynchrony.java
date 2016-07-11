package net.fortytwo.smsn;

import net.fortytwo.smsn.util.TypedProperties;

import java.io.File;
import java.io.FileInputStream;
import java.io.IOException;
import java.io.InputStream;
import java.util.Properties;
import java.util.Random;
import java.util.logging.Level;
import java.util.logging.LogManager;
import java.util.logging.Logger;

/**
 * @author Joshua Shinavier (http://fortytwo.net)
 */
public class SemanticSynchrony {
    public static final boolean
            SAFE = true,
            VERBOSE = true;

    // configuration properties
    public static final String
            BASE_URI = "net.fortytwo.smsn.baseURI",
            ACTIVITY_LOG = "net.fortytwo.smsn.activityLog",
            ATOM_NAMESPACE = "net.fortytwo.smsn.atomNamespace",
            VERSION = "net.fortytwo.smsn.version";

    // P2P configuration properties
    public static final String
            P2P_AGENT_IRI = "net.fortytwo.smsn.p2p.agentIri",
            P2P_BROADCAST_ADDRESS = "net.fortytwo.smsn.p2p.broadcastAddress",
            P2P_BROADCAST_PORT = "net.fortytwo.smsn.p2p.broadcastPort",
            P2P_BROADCAST_INTERVAL = "net.fortytwo.smsn.p2p.broadcastInterval",
            P2P_OSC_PORT = "net.fortytwo.smsn.p2p.oscPort",
            P2P_PUBSUB_PORT = "net.fortytwo.smsn.p2p.pubsubPort";

    // other service properties
    public static final String
            BRAIN_PORT = "net.fortytwo.smsn.server.brainPort";

    // core schema constants
    public static final String
            ALIAS = "alias",
            CREATED = "created",
            FIRST = "first",
            NOTES = "notes",
            REST = "rest",
            SHARABILITY = "sharability",
            VALUE = "value",
            WEIGHT = "weight";

    // extended schema constants
    public static final String
            ACRONYM = "acronym",
            PRIORITY = "priority",
            SHORTCUT = "shortcut";

    private static final int KEY_DIGITS = 7;

    private static final byte[] HEX_CHARS = "0123456789ABCDEF".getBytes();

    private static final Random random = new Random();

    private static final String
            DEFAULT_PROPERTIES = "smsn.properties",
            WORKING_DIR_PROPERTIES = "smsn.properties";

    public static final Logger logger;

    public static final String UTF8 = "UTF-8";

    public static final int
            GESTURE_TTL = 1, // we consider gestural events to be valid only for 1 second (the minimum TTL)
            ATTENTION_TTL = 5; // we consider attention to be valid for several seconds

    private static TypedProperties configuration;

    static {
        try {
            // logging configuration
            {
                try (InputStream in = SemanticSynchrony.class.getResourceAsStream("logging.properties")) {
                    LogManager.getLogManager().reset();
                    LogManager.getLogManager().readConfiguration(in);
                }
                logger = getLogger(SemanticSynchrony.class);
            }

            configuration = new TypedProperties();

            // first load SmSn's internal default properties
            configuration.load(SemanticSynchrony.class.getResourceAsStream(DEFAULT_PROPERTIES));

            // attempt to load additional properties from a user-provided file in the current directory
            File f = new File(WORKING_DIR_PROPERTIES);
            if (f.exists()) {
                addConfiguration(f);
            } else {
                logger.info("using default Semantic Synchrony configuration");
            }

            // further properties may be added later with addProperties()
        } catch (IOException e) {
            throw new ExceptionInInitializerError(e);
        }
    }

    public static Logger getLogger(final Class c) {
        return Logger.getLogger(c.getName());
    }

    /**
     * Adds the properties found at a given file path to the SmSn configuration.
     * These add to or replace SmSn's default properties and any properties found in an smsn.properties file
     * in the current directory.
     *
     * @param file the file path to the configuration properties to add
     * @throws java.io.IOException if the file does not exist or can't be loaded
     */
    public static void addConfiguration(final File file) throws IOException {
        if (file.exists()) {
            logger.info("loading Semantic Synchrony configuration at " + file.getAbsoluteFile());
            Properties p = new Properties();
            try (InputStream in = new FileInputStream(file)) {
                p.load(in);
            }

            addConfiguration(p);
        } else {
            throw new IOException("properties file does not exist: " + file);
        }
    }

    /**
     * Adds the given properties to the SmSn configuration.
     * These add to or replace SmSn's default properties and any properties found in an smsn.properties file
     * in the current directory.
     *
     * @param properties the new key/value pairs to add.
     *                   Note that a new value for a key already present in configuration will replace the old value
     */
    public static void addConfiguration(final Properties properties) {
        configuration.putAll(properties);
    }

    public static TypedProperties getConfiguration() {
        return configuration;
    }

    /**
     * Creates a pseudo-random Base64 SmSn key.
     * These keys are typically used as ids of atoms and list elements in Extend-o-Brain.
     *
     * @return a new pseudo-random key
     */
    public static String createRandomKey() {
        byte[] bytes = new byte[KEY_DIGITS];
        for (int i = 0; i < KEY_DIGITS; i++) {
            int n = random.nextInt(64);
            int b = n < 26
                    ? 'A' + n
                    : n < 52
                    ? 'a' + n - 26
                    : n < 62
                    ? '0' + n - 52
                    : n < 63
                    ? '-' : '_';
            bytes[i] = (byte) b;
        }

        return new String(bytes);
    }

    /**
     * Unicode-escapes strings for ease of consumption by external tools such as R.
     * Characters in high (0x7F or higher) and low (lower than 0x20) ranges are escaped.
     * Note that these ranges include newline, tab, and delete characters.
     *
     * @param plain the string to escape
     * @return the escaped string
     */
    public static String unicodeEscape(final String plain) {
        StringBuilder sb = new StringBuilder();
        for (int i = 0; i < plain.length(); i++) {
            char c = plain.charAt(i);
            if (c < 32 || c >= 127) {
                sb.append("\\u");
                sb.append((char) HEX_CHARS[(c >> 12) & 0xF]);
                sb.append((char) HEX_CHARS[(c >> 8) & 0xF]);
                sb.append((char) HEX_CHARS[(c >> 4) & 0xF]);
                sb.append((char) HEX_CHARS[c & 0xF]);
            } else {
                sb.append(c);
            }
        }
        return sb.toString();
    }

    public static void logInfo(final String message) {
        logger.log(Level.INFO, message);
    }

    public static void logWarning(final String message) {
        logger.log(Level.WARNING, message);
    }

    public static void logWarning(final String message,
                                  final Throwable thrown) {
        logger.log(Level.WARNING, message, thrown);
    }

    public static void logSevere(final String message,
                                 final Throwable thrown) {
        logger.log(Level.SEVERE, message, thrown);
    }
}
