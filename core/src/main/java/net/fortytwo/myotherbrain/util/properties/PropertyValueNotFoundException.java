package net.fortytwo.myotherbrain.util.properties;

/**
 * Author: josh
* Date: Jun 28, 2009
* Time: 6:08:59 PM
*/
public class PropertyValueNotFoundException extends PropertyException {
    public PropertyValueNotFoundException(final String propertyName) {
        super(propertyName);
    }
}
