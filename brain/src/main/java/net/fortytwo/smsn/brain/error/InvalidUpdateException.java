package net.fortytwo.smsn.brain.error;

/**
* @author Joshua Shinavier (http://fortytwo.net)
*/
public class InvalidUpdateException extends AtomGraphException {
    public InvalidUpdateException(final String message) {
        super(message);
    }
}
