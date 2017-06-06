package net.fortytwo.smsn.brain.error;

public class InvalidUpdateException extends RuntimeException {
    public InvalidUpdateException(final String message) {
        super(message);
    }
}
