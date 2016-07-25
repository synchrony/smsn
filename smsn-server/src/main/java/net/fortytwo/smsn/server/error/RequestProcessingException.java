package net.fortytwo.smsn.server.error;

/**
* @author Joshua Shinavier (http://fortytwo.net)
*/
public class RequestProcessingException extends Exception {
    public RequestProcessingException(String message) {
        super(message);
    }

    public RequestProcessingException(Throwable cause) {
        super(cause);
    }
}
