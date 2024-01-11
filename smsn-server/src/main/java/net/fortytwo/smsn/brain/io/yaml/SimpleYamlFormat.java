package net.fortytwo.smsn.brain.io.yaml;

import net.fortytwo.smsn.brain.io.Format;

public class SimpleYamlFormat extends Format {
    private static final SimpleYamlFormat instance = new SimpleYamlFormat();

    private SimpleYamlFormat() {
        super("YAML-simple", Type.FileBased, "yaml");
    }

    public static SimpleYamlFormat getInstance() {
        return instance;
    }
}
