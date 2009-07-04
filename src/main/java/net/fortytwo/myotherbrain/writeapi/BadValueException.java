package net.fortytwo.myotherbrain.writeapi;

/**
 * Author: josh
 * Date: Jul 3, 2009
 * Time: 6:49:48 PM
 */
public class BadValueException extends WriteException {
    public BadValueException(final String fieldURI,
                             final Object value) {
        super("bad value for field <" + fieldURI + ">: " + value);
    }
}
