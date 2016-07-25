package net.fortytwo.smsn.server.error;

/**
* @author Joshua Shinavier (http://fortytwo.net)
*/
public class BadRequestException extends Exception {
    public BadRequestException(String message) {
        super(message);
    }
}
