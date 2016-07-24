package net.fortytwo.smsn.brain.error;

/**
* @author Joshua Shinavier (http://fortytwo.net)
*/
public abstract class AtomGraphException extends RuntimeException {
    public AtomGraphException(final String message) {
        super(message);
    }
}
