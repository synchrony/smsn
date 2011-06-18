package net.fortytwo.myotherbrain.notes.diff;

/**
 * User: josh
 * Date: 6/13/11
 * Time: 11:57 PM
 */
public class AtomCreated {
    private final String id;
    private final String type;
    private final String name;
    private final String description;

    public AtomCreated(final String id,
                       final String type,
                       final String name,
                       final String description) {
        this.id = id;
        this.type = type;
        this.name = name;
        this.description = description;

        if (null == id || null == type || null == name || null == description) {
            throw new IllegalArgumentException();
        }
    }

    public String getId() {
        return id;
    }

    public String getType() {
        return type;
    }

    public String getName() {
        return name;
    }

    public String getDescription() {
        return description;
    }
}
