package net.fortytwo.myotherbrain.access.error;

/**
 * Author: josh
 * Date: Jun 30, 2009
 * Time: 9:55:22 PM
 */
public class BadEmailAddressException extends Exception {
    public BadEmailAddressException(final String message) {
        super(message);
    }
}
