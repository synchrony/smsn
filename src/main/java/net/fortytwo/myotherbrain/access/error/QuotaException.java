package net.fortytwo.myotherbrain.access.error;

import net.fortytwo.myotherbrain.update.UpdateException;

/**
 * Author: josh
* Date: May 8, 2009
* Time: 3:50:37 AM
*/
public class QuotaException extends UpdateException {
    public QuotaException(final String message) {
        super(message);
    }
}
