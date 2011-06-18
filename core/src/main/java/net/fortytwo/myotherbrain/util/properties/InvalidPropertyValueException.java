package net.fortytwo.myotherbrain.util.properties;

/**
 * Author: josh
* Date: Jun 28, 2009
* Time: 6:09:10 PM
*/
public class InvalidPropertyValueException extends PropertyException {
    public InvalidPropertyValueException(final String propertyName,
                                         final Throwable cause) {
        super(propertyName, cause);
    }
}
