package net.fortytwo.myotherbrain.update;

/**
 * User: josh
* Date: Sep 29, 2010
* Time: 11:29:27 PM
*/
public class MissingFieldException extends UpdateException {
    public MissingFieldException(final Enum field) {
        super("required field '" + field + "' is missing");
    }
}
