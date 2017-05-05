package net.fortytwo.smsn;

import net.fortytwo.smsn.config.Configuration;
import org.yaml.snakeyaml.Yaml;

import java.io.File;
import java.io.FileInputStream;
import java.io.IOException;
import java.io.InputStream;
import java.util.Random;
import java.util.logging.LogManager;
import java.util.logging.Logger;
import java.util.regex.Pattern;

public class SemanticSynchrony {

    public interface VertexLabels {
        String
                ATOM = "atom",
                LIST = "list",
                LINK = "link",
                PAGE = "page",
                TOPIC = "topic",
                TREE = "tree";
    }

    public interface EdgeLabels {
        String
                CHILDREN = "children",
                CONTEXT = "context",
                FIRST = "first",
                KEY = "key",
                NOTES = "notes",
                REST = "rest",
                TARGET = "target",
                TOPIC = "topic",
                CONTENT = "tree",
                VALUE = "value";
    }

    public interface PropertyKeys {
        String
                ACRONYM = "acronym",
                ALIAS = "alias",
                CREATED = "created",
                FORMAT = "format",
                ID_V = "idV",
                LABEL = "label",
                PAGE = "page",
                PRIORITY = "priority",
                SHARABILITY = "sharability",
                SHORTCUT = "shortcut",
                TEXT = "text",
                TITLE = "title",
                WEIGHT = "weight";
    }

    public interface Weight {
        float WEAK = 0.25f;
        float DEFAULT = 0.5f;
        float STRONG = 0.75f;
        float FULL = 1.0f;
    }

    public interface Sharability {
        float PRIVATE = 0.25f;
        float PERSONAL = 0.5f;
        float PUBLIC = 0.75f;
        float UNIVERSAL = 1.0f;
    }

    public interface Priority {
        float NONE = 0.0f;
        float LOW = 0.25f;
        float MEDIUM = 0.5f;
        float HIGH = 0.75f;
        float HIGHEST = 1.0f;
    }

    public static final Pattern ID_PATTERN = Pattern.compile("[a-zA-Z0-9-_]{7,}");

    private static final int ID_DIGITS = 16;

    private static final byte[] HEX_CHARS = "0123456789ABCDEF".getBytes();

    private static final Random random = new Random();

    private static final String
            SMSN_YAML = "smsn.yaml",
            SMSN_DEFAULT_YAML = "smsn-default.yaml";

    private static Logger logger;

    public static final String UTF8 = "UTF-8";

    public static final int
            GESTURE_TTL = 1, // we consider gestural events to be valid only for 1 second (the minimum TTL)
            ATTENTION_TTL = 5; // we consider attention to be valid for several seconds

    private static Configuration configuration;

    static {
        try {
            loadLoggingConfiguration();
            loadYamlFromDefaultLocation();
        } catch (IOException e) {
            throw new ExceptionInInitializerError(e);
        }
    }

    public static Logger getLogger() {
        return logger;
    }

    public static Logger getLogger(final Class c) {
        return Logger.getLogger(c.getName());
    }

    public static void readConfigurationYaml(final InputStream input) throws IOException {
        configuration = new Yaml().loadAs(input, Configuration.class);
    }

    public static Configuration getConfiguration() {
        return configuration;
    }

    private static void loadLoggingConfiguration() throws IOException {
        try (InputStream in = SemanticSynchrony.class.getResourceAsStream("logging.properties")) {
            if (null == in) throw new IllegalStateException();
            LogManager.getLogManager().reset();
            LogManager.getLogManager().readConfiguration(in);
        }
        logger = getLogger(SemanticSynchrony.class);
    }

    private static void loadYamlFromDefaultLocation() throws IOException {
        File f = new File(SMSN_YAML);
        if (f.exists() && !f.isDirectory() && f.canRead()) {
            logger.info("loading Semantic Synchrony configuration at " + f.getAbsolutePath());
            try (InputStream input = new FileInputStream(f)) {
                readConfigurationYaml(input);
            }
        } else {
            logger.info("using default Semantic Synchrony configuration");
            try (InputStream input = SemanticSynchrony.class.getResourceAsStream(SMSN_DEFAULT_YAML)) {
                readConfigurationYaml(input);
            }
        }
    }

    /**
     * Creates a pseudo-random Base62 SmSn key.
     * These keys are typically used as ids of atoms and list elements in Extend-o-Brain.
     *
     * @return a new pseudo-random key
     */
    public static String migrateId(final String original) {
        if (null != original) {
            random.setSeed(original.length() + original.hashCode());
        }

        byte[] bytes = new byte[ID_DIGITS];
        for (int i = 0; i < ID_DIGITS; i++) {
            int n = random.nextInt(62);
            int b = n < 26
                    ? 'A' + n
                    : n < 52
                    ? 'a' + n - 26
                    : '0' + n - 52;
            bytes[i] = (byte) b;
        }

        return new String(bytes);
    }

    public static String createRandomId() {
        return migrateId(null);
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
}
