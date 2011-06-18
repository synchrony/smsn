package net.fortytwo.myotherbrain.util.properties;

/**
 * Author: josh
* Date: Jun 28, 2009
* Time: 6:09:19 PM
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
