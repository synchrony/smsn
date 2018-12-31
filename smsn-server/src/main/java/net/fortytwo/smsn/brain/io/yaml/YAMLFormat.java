package net.fortytwo.smsn.brain.io.yaml;

import net.fortytwo.smsn.brain.io.Format;

public class YAMLFormat extends Format {

    public interface Constants {
        public static String ATOM = "Atom";

        public interface AtomFields {
            public static String ID = "id";
            public static String CREATED = "created";
            public static String WEIGHT = "weight";
            public static String PRIORITY = "priority";
            public static String TITLE = "title";
            public static String TEXT = "text";
            public static String ALIAS = "alias";
            public static String CHILDREN = "children";
            public static String SHORTCUT = "shortcut";
        }
    }

    public static final String YAML_EXTENSION = "yaml";

    private static final YAMLFormat instance = new YAMLFormat();

    private YAMLFormat() {
        super("YAML", Type.Complex, YAML_EXTENSION);
    }

    public static YAMLFormat getInstance() {
        return instance;
    }
}
