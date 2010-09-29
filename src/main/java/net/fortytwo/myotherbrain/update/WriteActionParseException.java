package net.fortytwo.myotherbrain.update;

/**
 * User: josh
 * Date: Sep 29, 2010
 * Time: 11:29:27 PM
 */
public class WriteActionParseException extends UpdateException {
    public WriteActionParseException(final String message) {
        super(message);
    }

    public static WriteActionParseException missingField(final Enum field) {
        return new WriteActionParseException("required field '" + field + "' is missing");        
    }

    /*
    public class MissingFieldException extends WriteActionParseException {
        public MissingFieldException(final Enum field) {
            super("required field '" + field + "' is missing");
        }
    }*/
}
