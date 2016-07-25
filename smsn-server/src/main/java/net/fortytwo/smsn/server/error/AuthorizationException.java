package net.fortytwo.smsn.server.error;

/**
* @author Joshua Shinavier (http://fortytwo.net)
*/
public class AuthorizationException extends Exception {
    public AuthorizationException(String message) {
        super(message);
    }
}
