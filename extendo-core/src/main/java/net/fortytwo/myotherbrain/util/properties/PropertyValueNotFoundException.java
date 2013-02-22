package net.fortytwo.myotherbrain.util.properties;

/**
 * @author Joshua Shinavier (http://fortytwo.net)
 */
public class PropertyValueNotFoundException extends PropertyException {
    public PropertyValueNotFoundException(final String propertyName) {
        super(propertyName);
    }
}
