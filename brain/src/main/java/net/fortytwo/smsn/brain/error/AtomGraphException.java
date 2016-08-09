package net.fortytwo.smsn.brain.error;

public abstract class AtomGraphException extends RuntimeException {
    public AtomGraphException(final String message) {
        super(message);
    }
}
