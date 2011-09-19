package net.fortytwo.myotherbrain.util.properties;

/**
 * @author Joshua Shinavier (http://fortytwo.net)
 */
public class PropertyException extends Exception {

    public PropertyException(final String propertyName) {
        super("for property '" + propertyName + "'");
    }

    public PropertyException(final String propertyName,
                             final Throwable cause) {
        super("for property '" + propertyName + "'", cause);
    }
}
