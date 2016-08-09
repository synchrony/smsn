package net.fortytwo.smsn.server.error;

public class AuthorizationException extends Exception {
    public AuthorizationException(String message) {
        super(message);
    }
}
