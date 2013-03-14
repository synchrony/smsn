package net.fortytwo.extendo.util.properties;

/**
 * @author Joshua Shinavier (http://fortytwo.net)
 */
public class InvalidPropertyValueException extends PropertyException {
    public InvalidPropertyValueException(final String propertyName,
                                         final Throwable cause) {
        super(propertyName, cause);
    }
}
