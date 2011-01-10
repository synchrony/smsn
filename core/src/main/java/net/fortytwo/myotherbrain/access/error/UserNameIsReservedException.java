package net.fortytwo.myotherbrain.access.error;

/**
 * Author: josh
 * Date: Jun 30, 2009
 * Time: 9:55:05 PM
 */
public class UserNameIsReservedException extends BadUserNameException {
    public UserNameIsReservedException(final String message) {
        super(message);
    }
}