package net.fortytwo.myotherbrain.update;

/**
 * Author: josh
 * Date: Jul 3, 2009
 * Time: 6:49:48 PM
 */
public class BadValueException extends UpdateException {
    public BadValueException(final String fieldURI,
                             final Object value) {
        super("bad value for field <" + fieldURI + ">: " + value);
    }
}
