package net.fortytwo.extendo;


import net.fortytwo.extendo.util.properties.TypedProperties;

import java.io.File;
import java.io.FileInputStream;
import java.io.IOException;
import java.io.InputStream;
import java.util.Random;
import java.util.logging.LogManager;
import java.util.logging.Logger;

/**
 * @author Joshua Shinavier (http://fortytwo.net)
 */
public class Extendo {
    public static final boolean
            SAFE = true,
            VERBOSE = false;

    // Configuration properties
    public static final String
            BASE_URI = "net.fortytwo.extendo.baseURI",
            ACTIVITY_LOG = "net.fortytwo.extendo.activityLog",
            VERSION = "net.fortytwo.extendo.version";

    // P2P configuration properties
    public static final String
            P2P_BROADCAST_ADDRESS = "net.fortytwo.extendo.p2p.broadcastAddress",
            P2P_BROADCAST_PORT = "net.fortytwo.extendo.p2p.broadcastPort",
            P2P_BROADCAST_INTERVAL = "net.fortytwo.extendo.p2p.broadcastInterval",
            P2P_OSC_PORT = "net.fortytwo.extendo.p2p.oscPort",
            P2P_PUBSUB_PORT = "net.fortytwo.extendo.p2p.pubsubPort";

    // other service properties
    public static final String
            BRAIN_PORT = "net.fortytwo.extendo.server.brainPort";

    // schema constants
    public static final String
            ALIAS = "alias",
            CREATED = "created",
            FIRST = "first",
            NOTE = "note",
            NOTES = "notes",
            PRIORITY = "priority",
            REST = "rest",
            SHARABILITY = "sharability",
            TYPE = "type",
            VALUE = "value",
            WEIGHT = "weight";

    private static final int KEY_DIGITS = 7;

    private static final byte[] HEX_CHARS = "0123456789ABCDEF".getBytes();

    private static final Random RANDOM = new Random();

    private static final String CONFIG_PROPERTIES_FILE = "extendo.properties";
    private static final Logger LOGGER;

    private static TypedProperties CONFIGURATION;

    static {
        try {
            {
                InputStream in = Extendo.class.getResourceAsStream("logging.properties");
                try {
                    LogManager.getLogManager().reset();
                    LogManager.getLogManager().readConfiguration(in);
                } finally {
                    in.close();
                }
            }

            LOGGER = getLogger(Extendo.class);

            CONFIGURATION = new TypedProperties();

            CONFIGURATION.load(Extendo.class.getResourceAsStream(CONFIG_PROPERTIES_FILE));

            // Attempt to load additional properties from a user-provided file in the current directory
            File f = new File(CONFIG_PROPERTIES_FILE);
            if (f.exists()) {
                LOGGER.info("loading Extendo configuration at " + f.getAbsoluteFile());
                InputStream in = new FileInputStream(f);
                try {
                    CONFIGURATION.load(in);
                } finally {
                    in.close();
                }
            } else {
                LOGGER.info("using default Extendo configuration");
            }
        } catch (IOException e) {
            throw new ExceptionInInitializerError(e);
        }
    }

    public static Logger getLogger(final Class c) {
        return Logger.getLogger(c.getName());
    }

    public static TypedProperties getConfiguration() {
        return CONFIGURATION;
    }

    /*
       For 5-digit numbers of base 64, expect a collision after 32768 trials (on average).
       There are 1,073,741,824 possibilities.

       int base = 64;
       int length = 5;
       BigDecimal poss = new BigDecimal(base).pow(length);
       BigDecimal trials = new BigDecimal(Math.sqrt((double) base)).pow(length);
       System.out.println("For " + length + "-digit numbers of base " + base + ", expect a collision after "
               + trials + " trials (on average).  There are " + poss + " possibilities.");
    */
    public static String createRandomKey() {
        byte[] bytes = new byte[KEY_DIGITS];
        for (int i = 0; i < KEY_DIGITS; i++) {
            int n = RANDOM.nextInt(64);
            int b = n < 26
                    ? 'A' + n
                    : n < 52
                    ? 'a' + n - 26
                    : n < 62
                    ? '0' + n - 52
                    : n < 63
                    ? '@' : '&';
            bytes[i] = (byte) b;
        }

        return new String(bytes);
    }

    // Note: escapes both high and low (whitespace < 0x20) characters.
    public static String unicodeEscape(final String s) {
        StringBuilder sb = new StringBuilder();
        for (int i = 0; i < s.length(); i++) {
            char c = s.charAt(i);
            if (c < 32 || (c >> 7) > 0) {
                sb.append("\\u");
                sb.append(HEX_CHARS[(c >> 12) & 0xF]);
                sb.append(HEX_CHARS[(c >> 8) & 0xF]);
                sb.append(HEX_CHARS[(c >> 4) & 0xF]);
                sb.append(HEX_CHARS[c & 0xF]);
            } else {
                sb.append(c);
            }
        }
        return sb.toString();
    }
}
