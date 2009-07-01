package net.fortytwo.myotherbrain.access.error;

import net.fortytwo.myotherbrain.access.error.BadUserNameException;

/**
 * Author: josh
 * Date: Jun 30, 2009
 * Time: 9:55:05 PM
 */
public class UserNameIsTooLongException extends BadUserNameException {
    public UserNameIsTooLongException(final String message) {
        super(message);
    }
}