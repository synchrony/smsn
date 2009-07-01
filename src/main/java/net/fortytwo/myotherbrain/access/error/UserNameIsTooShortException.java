package net.fortytwo.myotherbrain.access.error;

import net.fortytwo.myotherbrain.access.error.BadUserNameException;

/**
 * Author: josh
 * Date: Jun 30, 2009
 * Time: 9:55:05 PM
 */
public class UserNameIsTooShortException extends BadUserNameException {
    public UserNameIsTooShortException(final String message) {
        super(message);
    }
}