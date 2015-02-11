package net.fortytwo.extendo;

import net.fortytwo.extendo.util.TypedProperties;

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
public class Extendo {
    public static final boolean
            SAFE = true,
            VERBOSE = true;

    // configuration properties
    public static final String
            BASE_URI = "net.fortytwo.extendo.baseURI",
            ACTIVITY_LOG = "net.fortytwo.extendo.activityLog",
            ATOM_NAMESPACE = "net.fortytwo.extendo.atomNamespace",
            VERSION = "net.fortytwo.extendo.version";

    // P2P configuration properties
    public static final String
            P2P_AGENT_URI = "net.fortytwo.extendo.p2p.agentUri",
            P2P_BROADCAST_ADDRESS = "net.fortytwo.extendo.p2p.broadcastAddress",
            P2P_BROADCAST_PORT = "net.fortytwo.extendo.p2p.broadcastPort",
            P2P_BROADCAST_INTERVAL = "net.fortytwo.extendo.p2p.broadcastInterval",
            P2P_OSC_PORT = "net.fortytwo.extendo.p2p.oscPort",
            P2P_PUBSUB_PORT = "net.fortytwo.extendo.p2p.pubsubPort";

    // other service properties
    public static final String
            BRAIN_PORT = "net.fortytwo.extendo.server.brainPort";

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
            DEFAULT_PROPERTIES = "extendo.properties",
            WORKING_DIR_PROPERTIES = "extendo.properties";

    public static final Logger logger;

    public static final String UTF8 = "UTF-8";

    // we consider gesture to be useful only for 30s
    public static final long GESTURE_TTL = 30000;

    private static TypedProperties configuration;

    static {
        try {
            // logging configuration
            {
                InputStream in = Extendo.class.getResourceAsStream("logging.properties");
                try {
                    LogManager.getLogManager().reset();
                    LogManager.getLogManager().readConfiguration(in);
                } finally {
                    in.close();
                }
                logger = getLogger(Extendo.class);
            }

            configuration = new TypedProperties();

            // first load Extendo's internal default properties
            configuration.load(Extendo.class.getResourceAsStream(DEFAULT_PROPERTIES));

            // attempt to load additional properties from a user-provided file in the current directory
            File f = new File(WORKING_DIR_PROPERTIES);
            if (f.exists()) {
                addConfiguration(f);
            } else {
                logger.info("using default Extendo configuration");
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
     * Adds the properties found at a given file path to the Extendo configuration.
     * These add to or replace Extendo's default properties and any properties found in an extendo.properties file
     * in the current directory.
     *
     * @param file the file path to the configuration properties to add
     */
    public static void addConfiguration(final File file) throws IOException {
        if (file.exists()) {
            logger.info("loading Extendo configuration at " + file.getAbsoluteFile());
            Properties p = new Properties();
            InputStream in = new FileInputStream(file);
            try {
                p.load(in);
            } finally {
                in.close();
            }

            addConfiguration(p);
        } else {
            throw new IOException("properties file does not exist: " + file);
        }
    }

    /**
     * Adds the given properties to the Extendo configuration.
     * These add to or replace Extendo's default properties and any properties found in an extendo.properties file
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
     * Creates a pseudo-random Base64 Extendo key.
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
     * Characters in high (>= 0x7F) and low (< 0x20) ranges are escaped.
     * Note that these ranges include newline, tab, and delete characters.
     */
    public static String unicodeEscape(final String s) {
        StringBuilder sb = new StringBuilder();
        for (int i = 0; i < s.length(); i++) {
            char c = s.charAt(i);
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
