package net.fortytwo.myotherbrain.access.error;

/**
 * Author: josh
 * Date: Jun 30, 2009
 * Time: 9:55:05 PM
 */
public class EmailAddressIsAlreadyInUseException extends BadEmailAddressException {
    public EmailAddressIsAlreadyInUseException(final String message) {
        super(message);
    }
}