package net.fortytwo.myotherbrain.access.error;

/**
 * Author: josh
 * Date: Jun 30, 2009
 * Time: 9:55:13 PM
 */
public class BadPasswordException extends Exception {
    public BadPasswordException(final String message) {
        super(message);
    }
}
