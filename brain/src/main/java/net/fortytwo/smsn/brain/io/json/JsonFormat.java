package net.fortytwo.smsn.brain.io.json;

import net.fortytwo.smsn.brain.io.Format;

public class JsonFormat extends Format {

    public static final String TITLE_TRUNCATOR = " [...]";

    public interface Keys {
        String CHILDREN = "children";
        String NUMBER_OF_CHILDREN = "numberOfChildren";
        String NUMBER_OF_PARENTS = "numberOfParents";
        String ID = "id";
        String META = "meta";
    }

    private static final JsonFormat instance = new JsonFormat();

    private JsonFormat() {
        super("JSON", Type.Internal);
    }

    public static JsonFormat getInstance() {
        return instance;
    }
}
